with Interfaces; use Interfaces;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;

package util is

    function rawLongFloatBits is new Ada.Unchecked_Conversion(Long_Float, Unsigned_64);
    function rawFloatBits is new Ada.Unchecked_Conversion(Float, Unsigned_32);
    function toDouble is new Ada.Unchecked_Conversion(Unsigned_64, Long_Float);
    function fromFloatImmediate is new Ada.Unchecked_Conversion(Unsigned_32, Float);

    type HexDigitRange is range 0..15;
    type HexDigitsType is array (HexDigitRange) of Character;
    hexdigits : constant HexDigitsType := "0123456789ABCDEF";

    type DigitRange is range 0..9;
    type DigitsType is array (DigitRange) of Character;
    decdigits : constant DigitsType := "0123456789";

    subtype HexString32 is String (1..10);
    subtype HexString64 is String (1..18);
    subtype DecString32 is String (1..11);

    -- Convert unsigned integers to hex strings
    function toHexString(r : in Unsigned_32) return HexString32;
    function toHexString(r : in Unsigned_64) return HexString64;

    --function toString(r : Integer) return String;
    subtype byteOffset is Natural range 0..7;

    -- utility functions for setting certiain bytes in a word
    procedure setByte(byte : in byteOffset; val : in Unsigned_8; word : in out Unsigned_64);
    procedure setLoWord(val : in Unsigned_32; word : in out Unsigned_64);
    procedure setHiWord(val : in Unsigned_32; word : in out Unsigned_64);
    function getHiWord(word : in Unsigned_64) return Unsigned_32;
    function getLoWord(word : in Unsigned_64) return Unsigned_32;
    function getByte(byte : in byteOffset; word : in Unsigned_64) return Unsigned_8;

end util;
