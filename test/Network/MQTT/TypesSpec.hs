{-# LANGUAGE OverloadedStrings #-}

module Network.MQTT.TypesSpec where

import           Data.Binary           (decode, encode)
import           Data.ByteString.Lazy  (empty, pack)
import           Data.Either           (fromRight, isLeft, isRight)

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Network.MQTT.Types

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

spec :: Spec
spec = do
    -- Reference https://www.unicode.org/versions/Unicode11.0.0/ch03.pdf page 126
    describe "UTF-8 validator" $ do
        it "validates []" $ do
            utf8Validate empty `shouldBe` True

        it "invalidates [0x00]" $ do
            utf8Validate (pack [0x00]) `shouldBe` False

        prop "validates [0x01-7f]" $ forAll (choose (0x01, 0x7f)) $ \n -> do
            utf8Validate (pack [n]) `shouldBe` True


        prop "invalidates [0x80-0xc1, xx]" $ \n -> forAll (choose (0x80, 0xc1)) $ \m -> do
            utf8Validate (pack [m, n]) `shouldBe` False

        prop "invalidates [0xc2-df, 0x00-7f]" $ forAll (choose (0xc2, 0xdf)) $ \m -> forAll (choose (0x00, 0x7f)) $ \n -> do
            utf8Validate (pack [m, n]) `shouldBe` False

        prop "validates [0xc2-df, 0x80-bf]" $ forAll (choose (0xc2, 0xdf)) $ \m -> forAll (choose (0x80, 0xbf)) $ \n -> do
            utf8Validate (pack [m, n]) `shouldBe` True

        prop "invalidates [0xc2-df, 0xc0-ff]" $ forAll (choose (0xc2, 0xdf)) $ \m -> forAll (choose (0xc0, 0xff)) $ \n -> do
            utf8Validate (pack [m, n]) `shouldBe` False


        prop "invalidates [0xe0, 0x00-9f, xx]" $ \n -> forAll (choose (0x00, 0x9f)) $ \m -> do
            utf8Validate (pack [0xe0, m, n]) `shouldBe` False

        prop "invalidates [0xe0, 0xc0-ff, xx]" $ \n -> forAll (choose (0xc0, 0xff)) $ \m -> do
            utf8Validate (pack [0xe0, m, n]) `shouldBe` False

        prop "invalidates [0xe0, 0xa0-bf, 0x00-7f]" $ forAll (choose (0xa0, 0xbf)) $ \m -> forAll (choose (0x00, 0x7f)) $ \n -> do
            utf8Validate (pack [0xe0, m, n]) `shouldBe` False

        prop "invalidates [0xe0, 0xa0-bf, 0xc0-ff]" $ forAll (choose (0xa0, 0xbf)) $ \m -> forAll (choose (0xc0, 0xff)) $ \n -> do
            utf8Validate (pack [0xe0, m, n]) `shouldBe` False

        prop "validates [0xe0, 0xa0-bf, 0x80-bf]" $ forAll (choose (0xa0, 0xbf)) $ \m -> forAll (choose (0x80, 0xbf)) $ \n -> do
            utf8Validate (pack [0xe0, m, n]) `shouldBe` True


        prop "invalidates [0xe1-ec, 0x00-7f, xx]" $ \n -> forAll (choose (0xe1, 0xec)) $ \l -> forAll (choose (0x00, 0x7f)) $ \m -> do
            utf8Validate (pack [l, m, n]) `shouldBe` False

        prop "invalidates [0xe1-ec, 0xc0-ff, xx]" $ \n -> forAll (choose (0xe1, 0xec)) $ \l -> forAll (choose (0xc0, 0xff)) $ \m -> do
            utf8Validate (pack [l, m, n]) `shouldBe` False

        prop "invalidates [0xe1-ec, 0x80-bf, 0x00-7f]" $ forAll (choose (0xe1, 0xec)) $ \l -> forAll (choose (0x80, 0xbf)) $ \m -> forAll (choose (0x00, 0x7f)) $ \n -> do
            utf8Validate (pack [l, m, n]) `shouldBe` False

        prop "invalidates [0xe1-ec, 0x80-bf, 0xc0-ff]" $ forAll (choose (0xe1, 0xec)) $ \l -> forAll (choose (0x80, 0xbf)) $ \m -> forAll (choose (0xc0, 0xff)) $ \n -> do
            utf8Validate (pack [l, m, n]) `shouldBe` False

        prop "validates [0xe1-ec, 0x80-bf, 0x80-bf]" $ forAll (choose (0xe1, 0xec)) $ \l -> forAll (choose (0x80, 0xbf)) $ \m -> forAll (choose (0x80, 0xbf)) $ \n -> do
            utf8Validate (pack [l, m, n]) `shouldBe` True


        prop "invalidates [0xed, 0x00-7f, xx]" $ \n -> forAll (choose (0x00, 0x7f)) $ \m -> do
            utf8Validate (pack [0xed, m, n]) `shouldBe` False

        prop "invalidates [0xed, 0xa0-ff, xx]" $ \n -> forAll (choose (0xa0, 0xff)) $ \m -> do
            utf8Validate (pack [0xed, m, n]) `shouldBe` False

        prop "invalidates [0xed, 0x80-9f, 0x00-7f]" $ forAll (choose (0x80, 0x9f)) $ \m -> forAll (choose (0x00, 0x7f)) $ \n -> do
            utf8Validate (pack [0xed, m, n]) `shouldBe` False

        prop "invalidates [0xed, 0x80-9f, 0xc0-ff]" $ forAll (choose (0x80, 0x9f)) $ \m -> forAll (choose (0xc0, 0xff)) $ \n -> do
            utf8Validate (pack [0xed, m, n]) `shouldBe` False

        prop "validates [0xed, 0x80-9f, 0x80-bf]" $ forAll (choose (0x80, 0x9f)) $ \m -> forAll (choose (0x80, 0xbf)) $ \n -> do
            utf8Validate (pack [0xed, m, n]) `shouldBe` True


        prop "invalidates [0xee-ef, 0x00-7f, xx]" $ \n -> forAll (choose (0xee, 0xef)) $ \l -> forAll (choose (0x00, 0x7f)) $ \m -> do
            utf8Validate (pack [l, m, n]) `shouldBe` False

        prop "invalidates [0xee-ef, 0xc0-ff, xx]" $ \n -> forAll (choose (0xee, 0xef)) $ \l -> forAll (choose (0xc0, 0xff)) $ \m -> do
            utf8Validate (pack [l, m, n]) `shouldBe` False

        prop "invalidates [0xee-ef, 0x80-bf, 0x00-7f]" $ forAll (choose (0xee, 0xef)) $ \l -> forAll (choose (0x80, 0xbf)) $ \m -> forAll (choose (0x00, 0x7f)) $ \n -> do
            utf8Validate (pack [l, m, n]) `shouldBe` False

        prop "invalidates [0xee-ef, 0x80-bf, 0xc0-ff]" $ forAll (choose (0xee, 0xef)) $ \l -> forAll (choose (0x80, 0xbf)) $ \m -> forAll (choose (0xc0, 0xff)) $ \n -> do
            utf8Validate (pack [l, m, n]) `shouldBe` False

        prop "validates [0xee-ef, 0x80-bf, 0x80-bf]" $ forAll (choose (0xee, 0xef)) $ \l -> forAll (choose (0x80, 0xbf)) $ \m -> forAll (choose (0x80, 0xbf)) $ \n -> do
            utf8Validate (pack [l, m, n]) `shouldBe` True


        prop "invalidates [0xf0, 0x00-8f, xx, xx]" $ \n -> property $ \m -> forAll (choose (0x00, 0x8f)) $ \l -> do
            utf8Validate (pack [0xf0, l, m, n]) `shouldBe` False

        prop "invalidates [0xf0, 0xc0-ff, xx, xx]" $ \n -> property $ \m -> forAll (choose (0xc0, 0xff)) $ \l -> do
            utf8Validate (pack [0xf0, l, m, n]) `shouldBe` False

        prop "invalidates [0xf0, 0x90-bf, 0x00-7f, xx]" $ \n -> forAll (choose (0x90, 0xbf)) $ \l -> forAll (choose (0x00, 0x7f)) $ \m -> do
            utf8Validate (pack [0xf0, l, m, n]) `shouldBe` False

        prop "invalidates [0xf0, 0x90-bf, 0xc0-ff, xx]" $ \n -> forAll (choose (0x90, 0xbf)) $ \l -> forAll (choose (0xc0, 0xff)) $ \m -> do
            utf8Validate (pack [0xf0, l, m, n]) `shouldBe` False

        prop "invalidates [0xf0, 0x90-bf, 0x80-bf, 0x00-7f]" $ forAll (choose (0x90, 0xbf)) $ \l -> forAll (choose (0x80, 0xbf)) $ \m -> forAll (choose (0x00, 0x7f)) $ \n -> do
            utf8Validate (pack [0xf0, l, m, n]) `shouldBe` False

        prop "invalidates [0xf0, 0x90-bf, 0x80-bf, 0xc0-ff]" $ forAll (choose (0x90, 0xbf)) $ \l -> forAll (choose (0x80, 0xbf)) $ \m -> forAll (choose (0xc0, 0xff)) $ \n -> do
            utf8Validate (pack [0xf0, l, m, n]) `shouldBe` False

        prop "validates [0xf0, 0x90-bf, 0x80-bf, 0x80-bf]" $ forAll (choose (0x90, 0xbf)) $ \l -> forAll (choose (0x80, 0xbf)) $ \m -> forAll (choose (0x80, 0xbf)) $ \n -> do
            utf8Validate (pack [0xf0, l, m, n]) `shouldBe` True


        prop "invalidates [0xf1-f3, 0x00-7f, xx, xx]" $ \n -> property $ \m -> forAll (choose (0xf1, 0xf3)) $ \k -> forAll (choose (0x00, 0x7f)) $ \l -> do
            utf8Validate (pack [k, l, m, n]) `shouldBe` False

        prop "invalidates [0xf1-f3, 0xc0-ff, xx, xx]" $ \n -> property $ \m -> forAll (choose (0xf1, 0xf3)) $ \k -> forAll (choose (0xc0, 0xff)) $ \l -> do
            utf8Validate (pack [k, l, m, n]) `shouldBe` False

        prop "invalidates [0xf1-f3, 0x80-bf, 0x00-7f, xx]" $ \n -> forAll (choose (0xf1, 0xf3)) $ \k -> forAll (choose (0x80, 0xbf)) $ \l -> forAll (choose (0x00, 0x7f)) $ \m -> do
            utf8Validate (pack [k, l, m, n]) `shouldBe` False

        prop "invalidates [0xf1-f3, 0x80-bf, 0xc0-ff, xx]" $ \n -> forAll (choose (0xf1, 0xf3)) $ \k -> forAll (choose (0x80, 0xbf)) $ \l -> forAll (choose (0xc0, 0xff)) $ \m -> do
            utf8Validate (pack [k, l, m, n]) `shouldBe` False

        prop "invalidates [0xf1-f3, 0x80-bf, 0x80-bf, 0x00-7f]" $ forAll (choose (0xf1, 0xf3)) $ \k -> forAll (choose (0x80, 0xbf)) $ \l -> forAll (choose (0x80, 0xbf)) $ \m -> forAll (choose (0x00, 0x7f)) $ \n -> do
            utf8Validate (pack [k, l, m, n]) `shouldBe` False

        prop "invalidates [0xf1-f3, 0x80-bf, 0x80-bf, 0xc0-ff]" $ forAll (choose (0xf1, 0xf3)) $ \k -> forAll (choose (0x80, 0xbf)) $ \l -> forAll (choose (0x80, 0xbf)) $ \m -> forAll (choose (0xc0, 0xff)) $ \n -> do
            utf8Validate (pack [k, l, m, n]) `shouldBe` False

        prop "validates [0xf1-f3, 0x80-bf, 0x80-bf, 0x80-bf]" $ forAll (choose (0xf1, 0xf3)) $ \k -> forAll (choose (0x80, 0xbf)) $ \l -> forAll (choose (0x80, 0xbf)) $ \m -> forAll (choose (0x80, 0xbf)) $ \n -> do
            utf8Validate (pack [k, l, m, n]) `shouldBe` True


        prop "invalidates [0xf4, 0x00-8f, xx, xx]" $ \n -> property $ \m -> forAll (choose (0x00, 0x7f)) $ \l -> do
            utf8Validate (pack [0xf4, l, m, n]) `shouldBe` False

        prop "invalidates [0xf4, 0xc0-ff, xx, xx]" $ \n -> property $ \m -> forAll (choose (0x90, 0xff)) $ \l -> do
            utf8Validate (pack [0xf4, l, m, n]) `shouldBe` False

        prop "invalidates [0xf4, 0x90-bf, 0x00-7f, xx]" $ \n -> forAll (choose (0x80, 0x8f)) $ \l -> forAll (choose (0x00, 0x7f)) $ \m -> do
            utf8Validate (pack [0xf4, l, m, n]) `shouldBe` False

        prop "invalidates [0xf4, 0x90-bf, 0xc0-ff, xx]" $ \n -> forAll (choose (0x80, 0x8f)) $ \l -> forAll (choose (0xc0, 0xff)) $ \m -> do
            utf8Validate (pack [0xf4, l, m, n]) `shouldBe` False

        prop "invalidates [0xf4, 0x90-bf, 0x80-bf, 0x00-7f]" $ forAll (choose (0x80, 0x8f)) $ \l -> forAll (choose (0x80, 0xbf)) $ \m -> forAll (choose (0x00, 0x7f)) $ \n -> do
            utf8Validate (pack [0xf4, l, m, n]) `shouldBe` False

        prop "invalidates [0xf4, 0x90-bf, 0x80-bf, 0xc0-ff]" $ forAll (choose (0x80, 0x8f)) $ \l -> forAll (choose (0x80, 0xbf)) $ \m -> forAll (choose (0xc0, 0xff)) $ \n -> do
            utf8Validate (pack [0xf4, l, m, n]) `shouldBe` False

        prop "validates [0xf4, 0x80-8f, 0x80-bf, 0x80-bf]" $ forAll (choose (0x80, 0x8f)) $ \l -> forAll (choose (0x80, 0xbf)) $ \m -> forAll (choose (0x80, 0xbf)) $ \n -> do
            utf8Validate (pack [0xf4, l, m, n]) `shouldBe` True

    describe "UTF-8 string" $ do
        it "encodes [0x41, 0xf0, 0xaa, 0x9b, 0x94] to [0x00, 0x05, 0x41, 0xf0, 0xaa, 0x9b, 0x94]" $ do
            case toUtf8String (pack [0x41, 0xf0, 0xaa, 0x9b, 0x94]) of
                Left _          -> error "Impossible happened!"
                Right utf8str   -> encode utf8str `shouldBe` pack [0x00, 0x05, 0x41, 0xf0, 0xaa, 0x9b, 0x94]

{-
    describe "PacketType" $ do
        it "Index of PacketTypeReserved0 is 0" $ do
            fromEnum PacketTypeReserved0 `shouldBe` 0

        it "Index of PacketTypeReserved15 is 0" $ do
            fromEnum PacketTypeReserved15 `shouldBe` 15

        it "PacketTypeConnect through PacketTypeDisconnect should have proper indecis" $ do
            fromEnum PacketTypeConnect `shouldBe` 1
            fromEnum PacketTypeConnAck `shouldBe` 2
            fromEnum PacketTypePulish `shouldBe` 3
            fromEnum PacketTypePubAck `shouldBe` 4
            fromEnum PacketTypePubRec `shouldBe` 5
            fromEnum PacketTypePubRel `shouldBe` 6
            fromEnum PacketTypePubComp `shouldBe` 7
            fromEnum PacketTypeSubscribe `shouldBe` 8
            fromEnum PacketTypeSubAck `shouldBe` 9
            fromEnum PacketTypeUnsubscribe `shouldBe` 10
            fromEnum PacketTypeUnsubAck `shouldBe` 11
            fromEnum PacketTypePingReq `shouldBe` 12
            fromEnum PacketTypePingResp `shouldBe` 13
            fromEnum PacketTypeDisconnect `shouldBe` 14

        it "headToPacketType 0x10 decodes PacketTypeConnect" $ do
            headToPacketType 0x10 `shouldBe` PacketTypeConnect

        it "headToPacketType 0xe0 decodes PacketTypeDisconnect" $ do
            headToPacketType 0xe0 `shouldBe` PacketTypeDisconnect
-}

    describe "Remaining Length" $ do
        prop "toRemainingLength returns Left for negative value" $ \(Positive n) -> do
            toRemainingLength (-n) `shouldSatisfy` isLeft

        prop "toRemainingLength returns Left for length bigger than 0x0fffffff" $ forAll (choose (0x10000000, 0x7fffffffffffffff)) $ \n -> do
            toRemainingLength n `shouldSatisfy` isLeft

        prop "toRemainingLength returns Right for length between 0 and 0x0fffffff" $ forAll (choose (0, 0x0fffffff)) $ \n -> do
            toRemainingLength n `shouldSatisfy` isRight

        it "encodes 0x00 to [0x00]" $ do
            case toRemainingLength 0x00 of
                Left  _ -> error "Impossible happened!"
                Right l -> encode l `shouldBe` pack [0x00]

        it "encodes 0x7f to [0x7f]" $ do
            case toRemainingLength 0x7f of
                Left  _ -> error "Impossible happened!"
                Right l -> encode l `shouldBe` pack [0x7f]

        it "encodes 0x80 to [0x80, 0x01]" $ do
            case toRemainingLength 0x80 of
                Left  _ -> error "Impossible happened!"
                Right l -> encode l `shouldBe` pack [0x80, 0x01]

        it "encodes 0x3fff to [0xff, 0x7f]" $ do
            case toRemainingLength 0x3fff of
                Left  _ -> error "Impossible happened!"
                Right l -> encode l `shouldBe` pack [0xff, 0x7f]

        it "encodes 0x4000 to [0x80, 0x80, 0x01]" $ do
            case toRemainingLength 0x4000 of
                Left  _ -> error "Impossible happened!"
                Right l -> encode l `shouldBe` pack [0x80, 0x80, 0x01]

        it "encodes 0x1fffff to [0xff, 0xff, 0x7f]" $ do
            case toRemainingLength 0x1fffff of
                Left  _ -> error "Impossible happened!"
                Right l -> encode l `shouldBe` pack [0xff, 0xff, 0x7f]

        it "encodes 0x200000 to [0x80, 0x80, 0x80, 0x01]" $ do
            case toRemainingLength 0x200000 of
                Left  _ -> error "Impossible happened!"
                Right l -> encode l `shouldBe` pack [0x80, 0x80, 0x80, 0x01]

        it "encodes 0x0fffffff to [0xff, 0xff, 0xff, 0x7f]" $ do
            case toRemainingLength 0x0fffffff of
                Left  _ -> error "Impossible happened!"
                Right l -> encode l `shouldBe` pack [0xff, 0xff, 0xff, 0x7f]

        it "decodes [0x00] to 0x00" $ do
            case toRemainingLength 0x00 of
                Left  _ -> error "Impossible happened!"
                Right l -> l `shouldBe` decode (pack [0x00])
        it "decodes [0x7f] to 0x7f" $ do
            case toRemainingLength 0x7f of
                Left  _ -> error "Impossible happened!"
                Right l -> l `shouldBe` decode (pack [0x7f])

        it "decodes [0x80, 0x01] to 0x80" $ do
            case toRemainingLength 0x80 of
                Left  _ -> error "Impossible happened!"
                Right l -> l `shouldBe` decode (pack [0x80, 0x01])

        it "decodes [0xff, 0x7f] to 0x3fff" $ do
            case toRemainingLength 0x3fff of
                Left  _ -> error "Impossible happened!"
                Right l -> l `shouldBe` decode (pack [0xff, 0x7f])

        it "decodes [0x80, 0x80, 0x01] to 0x4000" $ do
            case toRemainingLength 0x4000 of
                Left  _ -> error "Impossible happened!"
                Right l -> l `shouldBe` decode (pack [0x80, 0x80, 0x01])

        it "decodes [0xff, 0xff, 0x7f] to 0x1fffff" $ do
            case toRemainingLength 0x1fffff of
                Left  _ -> error "Impossible happened!"
                Right l -> l `shouldBe` decode (pack [0xff, 0xff, 0x7f])

        it "decodes [0x80, 0x80, 0x80, 0x01] to 0x200000" $ do
            case toRemainingLength 0x200000 of
                Left  _ -> error "Impossible happened!"
                Right l -> l `shouldBe` decode (pack [0x80, 0x80, 0x80, 0x01])

        it "decodes [0xff, 0xff, 0xff, 0x7f] to 0x0fffffff" $ do
            case toRemainingLength 0x0fffffff of
                Left  _ -> error "Impossible happened!"
                Right l -> l `shouldBe` decode (pack [0xff, 0xff, 0xff, 0x7f])

        prop "(encode . decode) should be id" $ forAll (choose (0, 0x0fffffff)) $ \n -> do
            case toRemainingLength n of
                Left  _ -> error "Impossible happened!"
                Right l -> (decode . encode) l `shouldBe` l

    describe "ControlPacket" $ do
        it "encodes Connect" $ do
            let conn = do
                    cid <- ClientID <$> toUtf8String "cid"
                    name <- UserName <$> toUtf8String "name"
                    pass <- toPassword $ pack [1, 2, 3, 4]
                    willTopic <- Topic <$> toUtf8String "a/b"
                    willMsg <- toWillMessage "will"
                    pure ConnectR
                        { connectRKeepAlive   = KeepAlive 10
                        , connectCleanSession = True
                        , connectRClientID    = cid
                        , connectRWillSpec    = Just $ WillSpec AtLeastOnce False willTopic willMsg
                        , connectRUser        = Just $ User name $ Just pass
                        }
            case conn of
                Left _     -> error "Impossible happened!"
                Right conn -> do
                        encode (Connect conn) `shouldBe` pack [ 0x10,38
                                                              , 0x00,0x04,0x4d,0x51,0x54,0x54
                                                              , 0x04, 0xce, 0x00,0x0a
                                                              , 0x00,0x03,0x63,0x69,0x64
                                                              , 0x00,0x03,0x61,0x2f,0x62
                                                              , 0x00,0x04,0x77,0x69,0x6c,0x6c
                                                              , 0x00,0x04,0x6e,0x61,0x6d,0x65
                                                              , 0x00,0x04,0x01,0x02,0x03,0x04 ]

        it "encodes ConnAck" $ do
            encode (ConnAck (ConnAckR (ConnAckFlags True) ConnectionRefusedUnacceptableProtocolVersion))
                `shouldBe` pack [0x20, 0x02, 0x01, 0x01]

        it "encodes Publish" $ do
            case Topic <$> toUtf8String "a/b" of
                Left _      -> error "Impossible happened!"
                Right topic -> do
                    let flag = PublishFlags True AtLeastOnce False
                        body = PublishR topic (PacketID 10) (pack [0x00, 0x01, 0x02, 0x03])
                    encode (Publish flag body) `shouldBe` pack [0x3a,0x0b,0x00,0x03,0x61,0x2f,0x62,0x00,0x0a,0x00,0x01,0x02,0x03]

        it "encodes PubAck (PacketID 0x89ab) to [0x40, 0x02, 0x89, 0xab]" $ do
            encode (PubAck $ PacketID 0x89ab) `shouldBe` pack [0x40, 0x02, 0x89, 0xab]

        it "encodes PubRec (PacketID 0x789a) to [0x50, 0x02, 0x78, 0x9a]" $ do
            encode (PubRec $ PacketID 0x789a) `shouldBe` pack [0x50, 0x02, 0x78, 0x9a]

        it "encodes PubRel (PacketID 0x6789) to [0x62, 0x02, 0x67, 0x89]" $ do
            encode (PubRel $ PacketID 0x6789) `shouldBe` pack [0x62, 0x02, 0x67, 0x89]

        it "encodes PubComp (PacketID 0x5678) to [0x70, 0x02, 0x56, 0x78]" $ do
            encode (PubComp $ PacketID 0x5678) `shouldBe` pack [0x70, 0x02, 0x56, 0x78]

        it "encodes Subscribe" $ do
            let eitherTopics = do
                    topic1 <- Topic <$> toUtf8String "a/b"
                    topic2 <- Topic <$> toUtf8String "c/d"
                    pure [TopicRequest topic1 AtLeastOnce, TopicRequest topic2 ExactOnce]
            case eitherTopics of
                Left _       -> error "Impossible happened!"
                Right topics -> do
                    encode (Subscribe (SubscribeR (PacketID 0x4567) topics))
                        `shouldBe` pack [0x82, 0x0e, 0x45, 0x67, 0x00, 0x03, 0x61, 0x2f, 0x62, 0x01, 0x00, 0x03, 0x63, 0x2f, 0x64, 0x02]

        it "encodes SubAck" $ do
            encode (SubAck (SubAckR (PacketID 0x3456) [SubscribeSuccessMaxQoS0, SubscribeSuccessMaxQoS2, SubscribeFailure]))
                `shouldBe` pack [0x90, 0x05, 0x34, 0x56, 0x00, 0x02, 0x80]

        it "encodes Unsubscribe" $ do
            let eitherTopics = do
                    topic1 <- Topic <$> toUtf8String "a/b"
                    topic2 <- Topic <$> toUtf8String "c/d"
                    pure [topic1, topic2]
            case eitherTopics of
                Left _       -> error "Impossible happened!"
                Right topics -> do
                    encode (Unsubscribe (UnsubscribeR (PacketID 0x2345) topics))
                        `shouldBe` pack [0xa2, 0x0c, 0x23, 0x45, 0x00, 0x03, 0x61, 0x2f, 0x62, 0x00, 0x03, 0x63, 0x2f, 0x64]

        it "encodes UnsubAck (PacketID 0x1234) to [0xb0, 0x02, 0x12, 0x34]" $ do
            encode (UnsubAck $ PacketID 0x1234) `shouldBe` pack [0xb0, 0x02, 0x12, 0x34]

        it "encodes PingReq to [0xc0, 0x00]" $ do
            encode PingReq `shouldBe` pack [0xc0, 0x00]

        it "encodes PingResp to [0xd0, 0x00]" $ do
            encode PingResp `shouldBe` pack [0xd0, 0x00]

        it "encodes Disconnect to [0xe0, 0x00]" $ do
            encode Disconnect `shouldBe` pack [0xe0, 0x00]

        it "decodes Connect" $ do
            let conn = do
                    cid <- ClientID <$> toUtf8String "cid"
                    name <- UserName <$> toUtf8String "name"
                    pass <- toPassword $ pack [1, 2, 3, 4]
                    willTopic <- Topic <$> toUtf8String "a/b"
                    willMsg <- toWillMessage "will"
                    pure ConnectR
                        { connectRKeepAlive   = KeepAlive 10
                        , connectCleanSession = True
                        , connectRClientID    = cid
                        , connectRWillSpec    = Just $ WillSpec AtLeastOnce False willTopic willMsg
                        , connectRUser        = Just $ User name $ Just pass
                        }
            case conn of
                Left _     -> error "Impossible happened!"
                Right conn -> do
                        Connect conn `shouldBe` decode (pack [ 0x10,38
                                                             , 0x00,0x04,0x4d,0x51,0x54,0x54
                                                             , 0x04, 0xce, 0x00,0x0a
                                                             , 0x00,0x03,0x63,0x69,0x64
                                                             , 0x00,0x03,0x61,0x2f,0x62
                                                             , 0x00,0x04,0x77,0x69,0x6c,0x6c
                                                             , 0x00,0x04,0x6e,0x61,0x6d,0x65
                                                             , 0x00,0x04,0x01,0x02,0x03,0x04 ])

        it "decodes ConnAck" $ do
            ConnAck (ConnAckR (ConnAckFlags True) ConnectionRefusedUnacceptableProtocolVersion)
                `shouldBe` decode (pack [0x20, 0x02, 0x01, 0x01])

        it "decodes Publish" $ do
            case Topic <$> toUtf8String "a/b" of
                Left _       -> error "Impossible happened!"
                Right topic  -> do
                    let flag = PublishFlags True AtLeastOnce False
                        body = PublishR topic (PacketID 10) (pack [0x00, 0x01, 0x02, 0x03])
                    Publish flag body `shouldBe` decode (pack [0x3a, 0x0b, 0x00, 0x03, 0x61, 0x2f, 0x62, 0x00, 0x0a, 0x00, 0x01, 0x02, 0x03])

        it "decodes PubAck (PacketID 0x89ab) to [0x40, 0x02, 0x89, 0xab]" $ do
            PubAck (PacketID 0x89ab) `shouldBe` decode (pack [0x40, 0x02, 0x89, 0xab])

        it "decodes PubRec (PacketID 0x789a) to [0x50, 0x02, 0x78, 0x9a]" $ do
            PubRec (PacketID 0x789a) `shouldBe` decode (pack [0x50, 0x02, 0x78, 0x9a])

        it "decodes PubRel (PacketID 0x6789) to [0x62, 0x02, 0x67, 0x89]" $ do
            PubRel (PacketID 0x6789) `shouldBe` decode (pack [0x62, 0x02, 0x67, 0x89])

        it "decodes PubComp (PacketID 0x5678) from [0x70, 0x02, 0x56, 0x78]" $ do
            PubComp (PacketID 0x5678) `shouldBe` decode (pack [0x70, 0x02, 0x56, 0x78])

        it "decodes Subscribe" $ do
            let eitherTopics = do
                    topic1 <- Topic <$> toUtf8String "a/b"
                    topic2 <- Topic <$> toUtf8String "c/d"
                    pure [TopicRequest topic1 AtLeastOnce, TopicRequest topic2 ExactOnce]
            case eitherTopics of
                Left _       -> error "Impossible happened!"
                Right topics -> do
                    Subscribe (SubscribeR (PacketID 0x4567) topics)
                        `shouldBe` decode (pack [0x82, 0x0e, 0x45, 0x67, 0x00, 0x03, 0x61, 0x2f, 0x62, 0x01, 0x00, 0x03, 0x63, 0x2f, 0x64, 0x02])

        it "decodes SubAck" $ do
            SubAck (SubAckR (PacketID 0x3456) [SubscribeSuccessMaxQoS0, SubscribeSuccessMaxQoS2, SubscribeFailure])
                `shouldBe` decode (pack [0x90, 0x05, 0x34, 0x56, 0x00, 0x02, 0x80])

        it "decodes Unsubscribe" $ do
            let maybeTopics = do
                    topic1 <- toUtf8String "a/b"
                    topic2 <- toUtf8String "c/d"
                    pure [Topic topic1, Topic topic2]
            case maybeTopics of
                Left _       -> error "Impossible happened!"
                Right topics -> do
                    Unsubscribe (UnsubscribeR (PacketID 0x2345) topics)
                        `shouldBe` decode (pack [0xa2, 0x0c, 0x23, 0x45, 0x00, 0x03, 0x61, 0x2f, 0x62, 0x00, 0x03, 0x63, 0x2f, 0x64])

        it "decodes UnsubAck (PacketID 0x1234) from [0xb0, 0x02, 0x12, 0x34]" $ do
            UnsubAck (PacketID 0x1234) `shouldBe` decode (pack [0xb0, 0x02, 0x12, 0x34])

        it "decodes PingReq from [0xc0, 0x00]" $ do
            PingReq `shouldBe` decode (pack [0xc0, 0x00])

        it "decodes PingResp from [0xd0, 0x00]" $ do
            PingResp `shouldBe` decode (pack [0xd0, 0x00])

        it "decodes Disconnect from [0xe0, 0x00]" $ do
            Disconnect `shouldBe` decode (pack [0xe0, 0x00])
