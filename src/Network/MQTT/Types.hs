{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Strict                     #-}

module Network.MQTT.Types where

import           Data.Binary          (Binary, decode, encode, get, put,
                                       putList)
import           Data.Binary.Get      (Get, getLazyByteString,
                                       getRemainingLazyByteString, getWord16be,
                                       getWord32be, getWord8, isEmpty)
import           Data.Binary.Put      (Put, putLazyByteString, putWord16be,
                                       putWord16le, putWord32be, putWord8)
import           Data.Bits            (clearBit, shift, testBit, (.&.), (.|.))
import           Data.ByteString.Lazy as BL (ByteString, foldl', length, pack)
import           Data.Foldable        (traverse_)
import           Data.Monoid          ((<>))
import           Data.Typeable        (Typeable, typeOf)
import           Data.Word            (Word16, Word32, Word8)
import           GHC.Generics         (Generic)
import           Text.Printf          (printf)

{-
    Naive implementation of UTF-8 validator.

    Reference
    https://www.unicode.org/versions/Unicode11.0.0/ch03.pdf page 126

    It invalidates 0x00 too.
-}

-- | UTF-8 validator state machine states.
data Utf8State
    = Utf8Init
    | Utf8Two2nd
    | Utf8ThreeA2nd
    | Utf8Final
    | Utf8ThreeB2nd
    | Utf8ThreeC2nd
    | Utf8ThreeD2nd
    | Utf8FourA2nd
    | Utf8FourA3rd
    | Utf8FourB2nd
    | Utf8FourB3rd
    | Utf8FourC2nd
    | Utf8FourC3rd
    | Utf8Invalid
    deriving (Eq)

-- | Transition validator state machine by received byte.
utf8NextState :: Utf8State -> Word8 -> Utf8State
utf8NextState Utf8Init c        | c == 0    = Utf8Invalid
                                | c < 0x80  = Utf8Init
                                | c < 0xc2  = Utf8Invalid
                                | c < 0xe0  = Utf8Two2nd
                                | c < 0xe1  = Utf8ThreeA2nd
                                | c < 0xed  = Utf8ThreeB2nd
                                | c < 0xee  = Utf8ThreeC2nd
                                | c < 0xf0  = Utf8ThreeD2nd
                                | c < 0xf1  = Utf8FourA2nd
                                | c < 0xf4  = Utf8FourB2nd
                                | c < 0xf5  = Utf8FourC2nd
                                | otherwise = Utf8Invalid
utf8NextState Utf8Two2nd c      | c < 0x80  = Utf8Invalid
                                | c < 0xc0  = Utf8Init
                                | otherwise = Utf8Invalid
utf8NextState Utf8ThreeA2nd c   | c < 0xa0  = Utf8Invalid
                                | c < 0xc0  = Utf8Final
                                | otherwise = Utf8Invalid
utf8NextState Utf8Final c    | c < 0x80  = Utf8Invalid
                                | c < 0xc0  = Utf8Init
                                | otherwise = Utf8Invalid
utf8NextState Utf8ThreeB2nd c   | c < 0x80  = Utf8Invalid
                                | c < 0xc0  = Utf8Final
                                | otherwise = Utf8Invalid
utf8NextState Utf8ThreeC2nd c   | c < 0x80  = Utf8Invalid
                                | c < 0xa0  = Utf8Final
                                | otherwise = Utf8Invalid
utf8NextState Utf8ThreeD2nd c   | c < 0x80  = Utf8Invalid
                                | c < 0xc0  = Utf8Final
                                | otherwise = Utf8Invalid
utf8NextState Utf8FourA2nd c    | c < 0x90  = Utf8Invalid
                                | c < 0xc0  = Utf8FourA3rd
                                | otherwise = Utf8Invalid
utf8NextState Utf8FourA3rd c    | c < 0x80  = Utf8Invalid
                                | c < 0xc0  = Utf8Final
                                | otherwise = Utf8Invalid
utf8NextState Utf8FourB2nd c    | c < 0x80  = Utf8Invalid
                                | c < 0xc0  = Utf8FourB3rd
                                | otherwise = Utf8Invalid
utf8NextState Utf8FourB3rd c    | c < 0x80  = Utf8Invalid
                                | c < 0xc0  = Utf8Final
                                | otherwise = Utf8Invalid
utf8NextState Utf8FourC2nd c    | c < 0x80  = Utf8Invalid
                                | c < 0x90  = Utf8FourC3rd
                                | otherwise = Utf8Invalid
utf8NextState Utf8FourC3rd c    | c < 0x80  = Utf8Invalid
                                | c < 0xc0  = Utf8Final
                                | otherwise = Utf8Invalid
utf8NextState Utf8Invalid _                 = Utf8Invalid

{-|
    Test if given ByteString contains valid UTF-8 string.
    Invalidates if the ByteString contains 0x00 byte.
-}
utf8Validate :: BL.ByteString -> Bool
utf8Validate s = BL.foldl' utf8NextState Utf8Init s == Utf8Init

newtype Utf8String = Utf8String BL.ByteString deriving (Eq, Show)

-- | Convert ByteString to validated MQTT UTF-8 string.
toUtf8String :: BL.ByteString -> Either String Utf8String
toUtf8String bs | BL.length bs < 0x10000 = if utf8Validate bs
                                           then Right $ Utf8String bs
                                           else Left $ printf "Invalid UTF-8 string %s." $ show bs
                | otherwise              = Left $ printf "ByteString length must be smaller than 0x10000 but was %d." $ BL.length bs

instance Binary Utf8String where
    get = do
        bs <- getWord16be >>= getLazyByteString . fromIntegral
        case toUtf8String bs of
            Left e    -> fail e
            Right str -> pure str

    put (Utf8String s) = putWord16be (fromIntegral $ BL.length s) *> putLazyByteString s


{-|
    Safe polymorphic enumeration decoder for enum types which consists of only sequential enum values.
    Decodes enumeration in Right when object binary contains valid enum value.
    Decodes Left when the binary contains unrecognized value.
-}
newtype SafeEnum a = SafeEnum (Either String a) deriving (Eq, Show)

getSafeEnum :: (Bounded a, Enum a, Typeable a, Integral i, Show i) => Get i -> Get (SafeEnum a)
getSafeEnum getIntegral = do
    w <- getIntegral
    let r = toEnum $ fromIntegral w
    pure $ if fromIntegral w <= fromEnum (maxBound `asTypeOf` r)
            then SafeEnum $ Right r
            else SafeEnum . Left $ getEnumError w r

getEnumError :: (Bounded a, Enum a, Typeable a, Show i) => i -> a -> String
getEnumError v t = "Decoded value " <> sv <> " is outside of enumeration " <> st <> " " <> sr
  where
    sv = show v
    st = show $ typeOf t
    sr = show (fromEnum (minBound `asTypeOf` t), fromEnum (maxBound `asTypeOf` t))

{-|
    Unsafe polymorphic enumeration decoder for enum types which consists of only sequential enum values.
    Decodes enumeration when object binary contains valid enum value.
    Fail decoder when the binary contains unrecognized value.
    User can catch failure using Data.Binary.decodeOrFail
-}
getEnum :: (Bounded a, Enum a, Typeable a, Integral i, Show i) => Get i -> Get a
getEnum getIntegral = do
    w <- getIntegral
    let r = toEnum $ fromIntegral w
    if fromIntegral w <= fromEnum (maxBound `asTypeOf` r)
    then pure r
    else fail $ getEnumError w r

getSafeEnum8 :: (Bounded a, Enum a, Typeable a) => Get (SafeEnum a)
getSafeEnum8 = getSafeEnum getWord8

getEnum8 :: (Bounded a, Enum a, Typeable a) => Get a
getEnum8 = getEnum getWord8

putEnum8 :: Enum a => a -> Put
putEnum8 = putWord8 . fromIntegral . fromEnum

{-|
    Encoding SafeEnum
    Generally you should not use these functions.  SafeEnum is not to be used for encoding but for decoding.
    You should use deterministic enums instead for encoding.
    You must not use putSafeEnum with Left value.  It doesn't encode properly.  It generates error.
-}
putSafeEnumError :: a
putSafeEnumError = error "SafeEnum containing Left cannot be used for encoding message"

putSafeEnum :: (Binary a) => SafeEnum a -> Put
putSafeEnum (SafeEnum (Right e)) = put e
putSafeEnum _                    = putSafeEnumError


{-
-- | MQTT Control Packet type in fixed header.
data PacketType
    = PacketTypeReserved0   -- ^ 0 Reserved
    | PacketTypeConnect     -- ^ 1 CONNECT
    | PacketTypeConnAck     -- ^ 2 CONNACK
    | PacketTypePulish      -- ^ 3 PUBLISH
    | PacketTypePubAck      -- ^ 4 PUBACK
    | PacketTypePubRec      -- ^ 5 PUBREC
    | PacketTypePubRel      -- ^ 6 PUBREL
    | PacketTypePubComp     -- ^ 7 PUBLCOMP
    | PacketTypeSubscribe   -- ^ 8 SUBSCRIBE
    | PacketTypeSubAck      -- ^ 9 SUBACK
    | PacketTypeUnsubscribe -- ^ 10 UNSUBSCRIBE
    | PacketTypeUnsubAck    -- ^ 11 UNSUBACK
    | PacketTypePingReq     -- ^ 12 PINGREQ
    | PacketTypePingResp    -- ^ 13 PINGRESP
    | PacketTypeDisconnect  -- ^ 14 DISCONNECT
    | PacketTypeReserved15  -- ^ 15 Reserved
    deriving (Enum, Eq, Show)

headToPacketType :: Word8 -> PacketType
headToPacketType = toEnum . fromIntegral . (`div` 16)
-}

newtype VariableByteInteger = VariableByteInteger Int deriving (Eq, Show)

instance Binary VariableByteInteger where
    get = do
        d0 <- fromIntegral <$> getWord8
        if d0 < 0x80
        then pure . VariableByteInteger $ d0
        else do
            d1 <- fromIntegral <$> getWord8
            if d1 < 0x80
            then pure . VariableByteInteger $ (d0 - 0x80) + d1 * 0x80
            else do
                d2 <- fromIntegral <$> getWord8
                if d2 < 0x80
                then pure . VariableByteInteger $ (d0 - 0x80) + (d1 - 0x80) * 0x80 + d2 * 0x4000
                else do
                    d3 <- fromIntegral <$> getWord8
                    if d3 < 0x80
                    then pure . VariableByteInteger $ (d0 - 0x80) + (d1 - 0x80) * 0x80 + (d2 - 0x80) * 0x4000 + d3 * 0x200000
                    else fail $ malformed d0 d1 d2 d3
      where
        malformed = printf "malformed remaining length %#04x %#04x %#04x %#04x"

    put (VariableByteInteger n) | n < 0x80      = putWord8 $ fromIntegral n
                                | n < 0x4000    = let d0 = fromIntegral $ (n `rem` 0x80) + 0x80
                                                      d1 = fromIntegral $ n `div` 0x80
                                                  in putWord8 d0 *> putWord8 d1
                                | n < 0x200000  = let d0 = fromIntegral $ (n `rem` 0x80) + 0x80
                                                      d1 = fromIntegral $ ((n `div` 0x80) `rem` 0x80) + 0x80
                                                      d2 = fromIntegral $ (n `div` 0x4000) `rem` 0x80
                                                  in putWord8 d0 *> putWord8 d1 *> putWord8 d2
                                | otherwise     = let d0 = fromIntegral $ (n `rem` 0x80) + 0x80
                                                      d1 = fromIntegral $ ((n `div` 0x80) `rem` 0x80) + 0x80
                                                      d2 = fromIntegral $ ((n `div` 0x4000) `rem` 0x80) + 0x80
                                                      d3 = fromIntegral $ (n `div` 0x200000) `rem` 0x80
                                                  in putWord8 d0 *> putWord8 d1 *> putWord8 d2 *> putWord8 d3

getVariableByteInteger :: Get VariableByteInteger
getVariableByteInteger = get

toVariableByteInteger :: Int -> Either String VariableByteInteger
toVariableByteInteger n | 0 <= n && n < 0x10000000  = Right (VariableByteInteger n)
                        | otherwise                 = Left $ show n <> " is greater than allowed remaining length"

fromVariableByteInteger :: VariableByteInteger -> Int
fromVariableByteInteger (VariableByteInteger n) = n

newtype BinaryData = BinaryData BL.ByteString deriving (Eq, Show)

-- | Factory function to construct valid BinaryData.
toBinaryData :: BL.ByteString -> Either String BinaryData
toBinaryData bs | BL.length bs < 0x10000 = Right $ BinaryData bs
                | otherwise              = Left $ printf "ByteString length must be smaller than 0x10000 but was %d." $ BL.length bs

instance Binary BinaryData where
    get = getWord16be >>= getLazyByteString . fromIntegral >>= pure . BinaryData
    put (BinaryData s) = putWord16be (fromIntegral $ BL.length s) *> putLazyByteString s

data PayloadFormat = PayloadFormatUnspecifiedBytes | PayloadFormatUtf8EncodedCharacterData deriving (Bounded, Enum, Eq, Show)

instance Binary PayloadFormat where
    get = getEnum8
    put = putEnum8

data QoS = AtMostOnce | AtLeastOnce | ExactOnce deriving (Bounded, Enum, Eq, Show)

instance Binary QoS where
    get = getEnum8
    put = putEnum8

newtype PayloadFormatIndicator = PayloadFormatIndicator PayloadFormat deriving newtype (Binary, Eq, Show)
newtype MessageExpiryInterval = MessageExpiryInterval Word32 deriving newtype (Binary, Eq, Show)
newtype ContentType = ContentType Utf8String deriving newtype (Binary, Eq, Show)
newtype ResponseTopic = ResponseTopic Utf8String deriving newtype (Binary, Eq, Show)
newtype CorrelationData = CorrelationData BinaryData deriving newtype (Binary, Eq, Show)
newtype SubscriptionIdentifier = SubscriptionIdentifier VariableByteInteger deriving newtype (Binary, Eq, Show)
newtype SessionExpiryInterval = SessionExpiryInterval Word32 deriving newtype (Binary, Eq, Show)
newtype AssignedClientIdentifier = AssignedClientIdentifier Utf8String deriving newtype (Binary, Eq, Show)
newtype ServerKeepAlive = ServerKeepAlive Word16 deriving newtype (Binary, Eq, Show)
newtype AuthenticationMethod = AuthenticationMethod Utf8String deriving newtype (Binary, Eq, Show)
newtype AuthenticationData = AuthenticationData BinaryData deriving newtype (Binary, Eq, Show)
newtype RequestProblemInformation = RequestProblemInformation Bool deriving newtype (Binary, Eq, Show)
newtype WillDelayInterval = WillDelayInterval Word32 deriving newtype (Binary, Eq, Show)
newtype RequestResponseInformation = RequestResponseInformation Bool deriving newtype (Binary, Eq, Show)
newtype ResponseInformation = ResponseInformation Utf8String deriving newtype (Binary, Eq, Show)
newtype ServerReference = ServerReference Utf8String deriving newtype (Binary, Eq, Show)
newtype ReasonString = ReasonString Utf8String deriving newtype (Binary, Eq, Show)
newtype ReceiveMaximum = ReceiveMaximum Word16 deriving newtype (Binary, Eq, Show)
newtype TopicAliasMaximum = TopicAliasMaximum Word16 deriving newtype (Binary, Eq, Show)
newtype TopicAlias = TopicAlias Word16 deriving newtype (Binary, Eq, Show)
newtype MaximumQoS = MaximumQoS QoS deriving newtype (Binary, Eq, Show)
newtype RetainAvailable = RetainAvailable Bool deriving newtype (Binary, Eq, Show)
newtype UserProperty = UserProperty Utf8String deriving newtype (Binary, Eq, Show)
newtype MaximumPacketSize = MaximumPacketSize Word32 deriving newtype (Binary, Eq, Show)
newtype WildcardSubscriptionAvailable = WildcardSubscriptionAvailable Bool deriving newtype (Binary, Eq, Show)
newtype SubscriptionIdentifierAvailable = SubscriptionIdentifierAvailable Bool deriving newtype (Binary, Eq, Show)
newtype SharedSubscriptionAvailable = SharedSubscriptionAvailable Bool deriving newtype (Binary, Eq, Show)

{-
data ConnFlags = ConnFlags
    { connFlagsUserName     :: Bool
    , connFlagsPassword     :: Bool
    , connFlagsWillRetain   :: Bool
    , connFlagsWillQoS      :: QoS
    , connFlagsWill         :: Bool
    , connFlagsCleanSession :: Bool
    } deriving (Eq, Show)

instance Binary ConnFlags where
    get = getWord8 >>= extract
      where
        extract byte = if validate
                       then pure $ ConnFlags name pass retain qos will clean
                       else fail $ printf "malformed Connect flags %#04x" byte
          where
            name = testBit byte 7
            pass = testBit byte 6
            retain = testBit byte 5
            qos = toEnum $ fromIntegral qosBits
            will = testBit byte 2
            clean = testBit byte 1
            qosBits = shift (byte .&. 0x18) (-3)
            validate = not (testBit byte 0) && qosBits < 3

    put (ConnFlags name pass retain qos will clean) = putWord8 $ nameBit .|. passBit .|. retainBit .|. qosBit .|. willBit .|. cleanBit
      where
        nameBit = if name then 0x80 else 0x00
        passBit = if pass then 0x40 else 0x00
        retainBit = if retain then 0x20 else 0x00
        qosBit = shift (fromIntegral $ fromEnum qos) 3
        willBit = if will then 0x04 else 0x00
        cleanBit = if clean then 0x02 else 0x00
-}

newtype KeepAlive = KeepAlive Word16 deriving newtype (Binary, Eq, Show)
newtype ClientID = ClientID Utf8String deriving newtype (Binary, Eq, Show)
newtype WillTopic = WillTopic Utf8String deriving newtype (Binary, Eq, Show)
newtype UserName = UserName Utf8String deriving newtype (Binary, Eq, Show)

toBin64k :: BL.ByteString -> Either String BL.ByteString
toBin64k bs | BL.length bs < 0x10000    = Right bs
            | otherwise                 = Left $ printf "ByteString length must be smaller than 0x10000 but was %d." $ BL.length bs

getBin64k :: Get BL.ByteString
getBin64k = getWord16be >>= getLazyByteString . fromIntegral

putBin64k :: BL.ByteString -> Put
putBin64k bs = do
        let len = BL.length bs
        if len < 0x10000
        then putWord16be (fromIntegral len) *> putLazyByteString bs
        else fail "cannot encode binary longer than 0xffff bytes"

newtype WillMessage = WillMessage BL.ByteString deriving (Eq, Show)

toWillMessage :: BL.ByteString -> Either String WillMessage
toWillMessage bs = WillMessage <$> toBin64k bs

instance Binary WillMessage where
    get = WillMessage <$> getBin64k
    put (WillMessage bs) = putBin64k bs

newtype Password = Password BL.ByteString deriving (Eq, Show)

toPassword :: BL.ByteString -> Either String Password
toPassword bs = Password <$> toBin64k bs

instance Binary Password where
    get = Password <$> getBin64k
    put (Password bs) = putBin64k bs

data WillSpec = WillSpec
    { willSpecQoS     :: QoS
    , willSpecRetain  :: Bool
    , willSpecTopic   :: Topic
    , willSpecMessage :: WillMessage
    } deriving (Eq, Show)

getWillSpec :: Word8 -> Get (Maybe WillSpec)
getWillSpec connFlags | testBit connFlags 2 = do    -- Check if Will Flag is True
                            let qosBits = shift (connFlags .&. 0x18) (-3)
                            if qosBits < 3
                            then do
                                let qos = toEnum $ fromIntegral qosBits
                                    retain = testBit connFlags 5
                                topic <- get
                                Just . WillSpec qos retain topic <$> get
                            else fail "connect flag contains invalid QoS"
                      | otherwise = pure Nothing


putWillSpec :: Maybe WillSpec -> Put
putWillSpec Nothing                             = pure ()
putWillSpec (Just (WillSpec _ _ topic message)) = put topic *> put message

setWillFlags :: Maybe WillSpec -> Word8
setWillFlags Nothing                            = 0x00
setWillFlags (Just (WillSpec qos retain _ _))   = retainBit .|. qosBits .|. 0x04
  where
    retainBit   = if retain then 0x20 else 0x00
    qosBits     = shift (fromIntegral $ fromEnum qos) 3

data User = User
    { userUserName :: UserName
    , userPassword :: Maybe Password
    } deriving (Eq, Show)

getUser :: Word8 -> Get (Maybe User)
getUser connFlags | testBit connFlags 7 = do    -- Check if User Name Flag is true
                        name <- get
                        if testBit connFlags 6  -- Check if Password Flag is true
                        then Just . User name . Just <$> get
                        else pure . Just $ User name Nothing
                  | otherwise = pure Nothing

putUser :: Maybe User -> Put
putUser Nothing                        = pure ()
putUser (Just (User name (Just pass))) = put name *> put pass
putUser (Just (User name Nothing))     = put name

setUserFlags :: Maybe User -> Word8
setUserFlags Nothing                  = 0x00
setUserFlags (Just (User _ (Just _))) = 0xc0
setUserFlags (Just (User _ Nothing))  = 0x80

data ConnectR = ConnectR
    { connectRKeepAlive   :: KeepAlive
    , connectCleanSession :: Bool
    , connectRClientID    :: ClientID
    , connectRWillSpec    :: Maybe WillSpec
    , connectRUser        :: Maybe User
    -- , connectRConnFlags   :: ConnFlags
    } deriving (Eq, Show)

connectProtocol :: BL.ByteString
connectProtocol = "\NUL\EOTMQTT\EOT"

instance Binary ConnectR where
    get = do
        proto <- getLazyByteString 7
        if proto /= connectProtocol
        then fail "invalid protocol name or level"
        else do
            connFlags <- getWord8
            let clean = testBit connFlags 1     -- Check of Clean Session Flag is true
            ka <- get
            cid <- get
            will <- getWillSpec connFlags
            ConnectR ka clean cid will <$> getUser connFlags

    put (ConnectR ka clean cid will user) = do
        putLazyByteString connectProtocol
        putWord8 $ setWillFlags will .|. setUserFlags user .|. setCleanFlag clean
        put ka *> put cid *> putWillSpec will *> putUser user
      where
        setCleanFlag False = 0x00
        setCleanFlag True  = 0x02

newtype ConnAckFlags = ConnAckFlags
    { connAckFlagsSessionPresent :: Bool
    } deriving (Eq, Show)

instance Binary ConnAckFlags where
    get = getWord8 >>= \b -> pure $ ConnAckFlags (testBit b 0)
    put (ConnAckFlags sp) = putWord8 $ if sp then 1 else 0

data ConnAckReturnCode
    = ConnectionAccepted
    | ConnectionRefusedUnacceptableProtocolVersion
    | ConnectionRefusedIdentifierRejected
    | ConnectionRefusedServerUnavailable
    | ConnectionRefusedBadUserNameOrPassword
    | ConnectionRefusedNotAuthorized
    deriving (Bounded, Enum, Eq, Show)

instance Binary ConnAckReturnCode where
    get = getEnum8
    put = putEnum8

data ConnAckR = ConnAckR
    { connAckRFlags      :: ConnAckFlags
    , connAckRReturnCode :: ConnAckReturnCode
    } deriving (Binary, Generic, Eq, Show)

newtype PacketID = PacketID Word16 deriving newtype (Binary, Eq, Show)

newtype Topic = Topic Utf8String deriving newtype (Binary, Eq, Show)

data PublishFlags = PublishFlags
    { publishFlagsDup    :: Bool
    , publishFlagsQoS    :: QoS
    , publishFlagsRetain :: Bool
    } deriving (Eq, Show)

decodePublishFlags :: Word8 -> Either String PublishFlags
decodePublishFlags header = if validateQoS
                            then Right $ PublishFlags dup qos retain
                            else Left $ printf "malformed Publish header %#04x" header
  where
    dup = testBit header 3
    qos = toEnum $ fromIntegral qosBits
    retain = testBit header 0
    qosBits = shift (header .&. 0x06) (-1)
    validateQoS = qosBits < 3

encodePublishHeader :: PublishFlags -> Word8
encodePublishHeader (PublishFlags dup qos retain) = 0x30 .|. dupBit .|. qosBit .|. retainBit
  where
    dupBit = if dup then 0x08 else 0x00
    qosBit = shift (fromIntegral $ fromEnum qos) 1
    retainBit = if retain then 0x01 else 0x00

data PublishR = PublishR
    { publishRTopic    :: Topic
    , publishRPacketID :: PacketID
    , publishPayload   :: BL.ByteString
    } deriving (Eq, Show)

instance Binary PublishR where
    get = do
        topic <- get
        pid <- get
        PublishR topic pid <$> getRemainingLazyByteString
    put (PublishR topic pid payload) = put topic *> put pid *> putLazyByteString payload

data TopicRequest = TopicRequest Topic QoS deriving (Eq, Generic, Show)

instance Binary TopicRequest where
    putList = traverse_ put

data SubscribeR = SubscribeR
    { subscribeRPacketID      :: PacketID
    , subscribeRTopicRequests :: [TopicRequest]
    } deriving (Eq, Generic, Show)

getList :: Binary a => Get [a]
getList = do
    finished <- isEmpty
    if finished
    then pure []
    else do
        headElement <- get
        tailElements <- getList
        pure (headElement:tailElements)

instance Binary SubscribeR where
    get = do
        pid <- get
        SubscribeR pid <$> getList

data SubAckReturnCode
    = SubscribeSuccessMaxQoS0
    | SubscribeSuccessMaxQoS1
    | SubscribeSuccessMaxQoS2
    | SubscribeFailure
    deriving (Bounded, Enum, Eq, Show)

instance Binary SubAckReturnCode where
    get = do
        byte <- getWord8
        case byte of
            n   | n < 0x03  -> pure . toEnum $ fromIntegral n
                | n == 0x80 -> pure SubscribeFailure
                | otherwise -> fail $ "Invalid SUBACK return code " <> show n

    put c   | c == SubscribeFailure = putWord8 0x80
            | otherwise             = putEnum8 c

data SubAckR = SubAckR
    { subAckRPacketID         :: PacketID
    , subAckRSubAckReturnCode :: [SubAckReturnCode]
    } deriving (Eq, Show)

instance Binary SubAckR where
    get = do
        pid <- get
        SubAckR pid <$> getList
    put (SubAckR pid codes) = put pid *> traverse_ put codes

data UnsubscribeR = UnsubscribeR
    { unsubscribeRPacketID     :: PacketID
    , unsubscribeRTopicFilters :: [Topic]
    } deriving (Eq, Show)

instance Binary UnsubscribeR where
    get = do
        pid <- get
        UnsubscribeR pid <$> getList
    put (UnsubscribeR pid topics) = put pid *> traverse_ put topics

data DisconnectReason
    = DisconnectReasonNormalDisconnection
    | DisconnectReasonDisconnectWithWillMessage
    | DisconnectReasonUnspecifiedError
    | DisconnectReasonMalformedPacket
    | DisconnectReasonProtocolError
    | DisconnectReasonImplementationSpecificError
    | DisconnectReasonNotAuthorized
    | DisconnectReasonServerBusy
    | DisconnectReasonServerShuttingDown
    | DisconnectReasonBadAuthenticationMethod
    | DisconnectReasonKeepAliveTimeout
    | DisconnectReasonSessionTakenOver
    | DisconnectReasonTopicFilterInvalid
    | DisconnectReasonTopicNameInvalid
    | DisconnectReasonReceiveMaximumExceeded
    | DisconnectReasonTopicAliasInvalid
    | DisconnectReasonPacketTooLarge
    | DisconnectReasonMessageRateTooHigh
    | DisconnectReasonQuotaExceeded
    | DisconnectReasonAdministrativeAction
    | DisconnectReasonPayloadFormatInvalid
    | DisconnectReasonRetainNotSupported
    | DisconnectReasonQoSNotSupported
    | DisconnectReasonUseAnotherServer
    | DisconnectReasonServerMoved
    | DisconnectReasonSharedSubscriptionsNotSupported
    | DisconnectReasonConnectionRateExceeded
    | DisconnectReasonMaximumConnectTime
    | DisconnectReasonSubscriptionIdentifiersNotSupported
    | DisconnectReasonWildcardSubscriptionsNotSupported

instance Binary DisconnectReason where
    get = do
        byte <- getWord8
        case byte of
            n | n == 0x00 -> pure DisconnectReasonNormalDisconnection
            n | n == 0x04 -> pure DisconnectReasonDisconnectWithWillMessage
            n | n == 0x80 -> pure DisconnectReasonUnspecifiedError
            n | n == 0x81 -> pure DisconnectReasonMalformedPacket
            n | n == 0x82 -> pure DisconnectReasonProtocolError
            n | n == 0x83 -> pure DisconnectReasonImplementationSpecificError
            n | n == 0x87 -> pure DisconnectReasonNotAuthorized
            n | n == 0x89 -> pure DisconnectReasonServerBusy
            n | n == 0x8b -> pure DisconnectReasonServerShuttingDown
            n | n == 0x8c -> pure DisconnectReasonBadAuthenticationMethod
            n | n == 0x8d -> pure DisconnectReasonKeepAliveTimeout
            n | n == 0x8e -> pure DisconnectReasonSessionTakenOver
            n | n == 0x8f -> pure DisconnectReasonTopicFilterInvalid
            n | n == 0x90 -> pure DisconnectReasonTopicNameInvalid
            n | n == 0x93 -> pure DisconnectReasonReceiveMaximumExceeded
            n | n == 0x94 -> pure DisconnectReasonTopicAliasInvalid
            n | n == 0x95 -> pure DisconnectReasonPacketTooLarge
            n | n == 0x96 -> pure DisconnectReasonMessageRateTooHigh
            n | n == 0x97 -> pure DisconnectReasonQuotaExceeded
            n | n == 0x98 -> pure DisconnectReasonAdministrativeAction
            n | n == 0x99 -> pure DisconnectReasonPayloadFormatInvalid
            n | n == 0x9a -> pure DisconnectReasonRetainNotSupported
            n | n == 0x9b -> pure DisconnectReasonQoSNotSupported
            n | n == 0x9c -> pure DisconnectReasonUseAnotherServer
            n | n == 0x9d -> pure DisconnectReasonServerMoved
            n | n == 0x9e -> pure DisconnectReasonSharedSubscriptionsNotSupported
            n | n == 0x9f -> pure DisconnectReasonConnectionRateExceeded
            n | n == 0xa0 -> pure DisconnectReasonMaximumConnectTime
            n | n == 0xa1 -> pure DisconnectReasonSubscriptionIdentifiersNotSupported
            n | n == 0xa2 -> pure DisconnectReasonWildcardSubscriptionsNotSupported
              | otherwise -> fail $ "Invalid disconnect reason code " <> show n

    put DisconnectReasonNormalDisconnection                 = putWord8 0x00
    put DisconnectReasonDisconnectWithWillMessage           = putWord8 0x04
    put DisconnectReasonUnspecifiedError                    = putWord8 0x80
    put DisconnectReasonMalformedPacket                     = putWord8 0x81
    put DisconnectReasonProtocolError                       = putWord8 0x82
    put DisconnectReasonImplementationSpecificError         = putWord8 0x83
    put DisconnectReasonNotAuthorized                       = putWord8 0x87
    put DisconnectReasonServerBusy                          = putWord8 0x89
    put DisconnectReasonServerShuttingDown                  = putWord8 0x8b
    put DisconnectReasonBadAuthenticationMethod             = putWord8 0x8c
    put DisconnectReasonKeepAliveTimeout                    = putWord8 0x8d
    put DisconnectReasonSessionTakenOver                    = putWord8 0x8e
    put DisconnectReasonTopicFilterInvalid                  = putWord8 0x8f
    put DisconnectReasonTopicNameInvalid                    = putWord8 0x90
    put DisconnectReasonReceiveMaximumExceeded              = putWord8 0x93
    put DisconnectReasonTopicAliasInvalid                   = putWord8 0x94
    put DisconnectReasonPacketTooLarge                      = putWord8 0x95
    put DisconnectReasonMessageRateTooHigh                  = putWord8 0x96
    put DisconnectReasonQuotaExceeded                       = putWord8 0x97
    put DisconnectReasonAdministrativeAction                = putWord8 0x98
    put DisconnectReasonPayloadFormatInvalid                = putWord8 0x99
    put DisconnectReasonRetainNotSupported                  = putWord8 0x9a
    put DisconnectReasonQoSNotSupported                     = putWord8 0x9b
    put DisconnectReasonUseAnotherServer                    = putWord8 0x9c
    put DisconnectReasonServerMoved                         = putWord8 0x9d
    put DisconnectReasonSharedSubscriptionsNotSupported     = putWord8 0x9e
    put DisconnectReasonConnectionRateExceeded              = putWord8 0x9f
    put DisconnectReasonMaximumConnectTime                  = putWord8 0xa0
    put DisconnectReasonSubscriptionIdentifiersNotSupported = putWord8 0xa1
    put DisconnectReasonWildcardSubscriptionsNotSupported   = putWord8 0xa2

data DisconnectR = DisconnectR
    { disconnectRReason :: DisconnectReason

    }

data ControlPacket
    = Connect ConnectR
    | ConnAck ConnAckR
    | Publish PublishFlags PublishR
    | PubAck PacketID
    | PubRec PacketID
    | PubRel PacketID
    | PubComp PacketID
    | Subscribe SubscribeR
    | SubAck SubAckR
    | Unsubscribe UnsubscribeR
    | UnsubAck PacketID
    | PingReq
    | PingResp
    | Disconnect
    deriving (Eq, Show)

putLenBody :: Binary a => Word8 -> a -> Put
putLenBody packetType body = do
    putWord8 packetType
    let bodyBytes = encode body
        eitherLen = toVariableByteInteger . fromIntegral $ BL.length bodyBytes
    case eitherLen of
        Left e    -> fail e
        Right len -> put len *> putLazyByteString bodyBytes

instance Binary ControlPacket where
    get = do
        header <- getWord8
        remainingLength <- getVariableByteInteger
        let body = getLazyByteString . fromIntegral $ fromVariableByteInteger remainingLength
        case header `div` 16 of
            0  -> fail "MQTT Packet Type number 0 is reserved"
            1  -> Connect . decode <$> body
            2  -> ConnAck . decode <$> body
            3  -> case decodePublishFlags header of
                    Right flags -> Publish flags . decode <$> body
                    Left e      -> fail e
            4  -> PubAck . decode <$> body
            5  -> PubRec . decode <$> body
            6  -> PubRel . decode <$> body
            7  -> PubComp . decode <$> body
            8  -> Subscribe . decode <$> body
            9  -> SubAck . decode <$> body
            10 -> Unsubscribe . decode <$> body
            11 -> UnsubAck . decode <$> body
            12 -> pure PingReq
            13 -> pure PingResp
            14 -> pure Disconnect
            15 -> fail "MQTT Packet Type number 15 is reserved"

    put (Connect body)       = putLenBody 0x10 body
    put (ConnAck varH)       = putWord16le 0x0220 *> put varH
    put (Publish flags body) = putLenBody (encodePublishHeader flags) body
    put (PubAck pid)         = putWord16le 0x0240 *> put pid
    put (PubRec pid)         = putWord16le 0x0250 *> put pid
    put (PubRel pid)         = putWord16le 0x0262 *> put pid
    put (PubComp pid)        = putWord16le 0x0270 *> put pid
    put (Subscribe body)     = putLenBody 0x82 body
    put (SubAck body)        = putLenBody 0x90 body
    put (Unsubscribe body)   = putLenBody 0xa2 body
    put (UnsubAck pid)       = putWord16le 0x02b0 *> put pid
    put PingReq              = putWord16le 0x00c0
    put PingResp             = putWord16le 0x00d0
    put Disconnect           = putWord16le 0x00e0
