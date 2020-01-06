package body util is

   function toHexString(r : in Unsigned_32) return HexString32 is
        ret : HexString32 := "0xDEADBEEF";
    begin
        ret(1) := '0';
        ret(2) := 'x';
        ret(3) := hexdigits(HexDigitRange(Shift_Right((r and 16#F000_0000#), 28)));
        ret(4) := hexdigits(HexDigitRange(Shift_Right((r and 16#0F00_0000#), 24)));
        ret(5) := hexdigits(HexDigitRange(Shift_Right((r and 16#00F0_0000#), 20)));
        ret(6) := hexdigits(HexDigitRange(Shift_Right((r and 16#000F_0000#), 16)));
        ret(7) := hexdigits(HexDigitRange(Shift_Right((r and 16#0000_F000#), 12)));
        ret(8) := hexdigits(HexDigitRange(Shift_Right((r and 16#0000_0F00#), 8)));
        ret(9) := hexdigits(HexDigitRange(Shift_Right((r and 16#0000_00F0#), 4)));
        ret(10) := hexdigits(HexDigitRange(r and 16#0000_000F#));
        return ret;
    end toHexString;

    -- Convert Unsigned_64 integer to a hex string
    function toHexString(r : in Unsigned_64) return HexString64 is
        ret : HexString64 := "0xDEADBEEFDEADBEEF";
    begin
        ret(1) := '0';
        ret(2) := 'x';
        ret(3) := hexdigits(HexDigitRange(Shift_Right((r and 16#F000_0000_0000_0000#), 60)));
        ret(4) := hexdigits(HexDigitRange(Shift_Right((r and 16#0F00_0000_0000_0000#), 56)));
        ret(5) := hexdigits(HexDigitRange(Shift_Right((r and 16#00F0_0000_0000_0000#), 52)));
        ret(6) := hexdigits(HexDigitRange(Shift_Right((r and 16#000F_0000_0000_0000#), 48)));
        ret(7) := hexdigits(HexDigitRange(Shift_Right((r and 16#0000_F000_0000_0000#), 44)));
        ret(8) := hexdigits(HexDigitRange(Shift_Right((r and 16#0000_0F00_0000_0000#), 40)));
        ret(9) := hexdigits(HexDigitRange(Shift_Right((r and 16#0000_00F0_0000_0000#), 36)));
        ret(10) := hexdigits(HexDigitRange(Shift_Right((r and 16#0000_000F_0000_0000#), 32)));
        ret(11) := hexdigits(HexDigitRange(Shift_Right((r and 16#0000_0000_F000_0000#), 28)));
        ret(12) := hexdigits(HexDigitRange(Shift_Right((r and 16#0000_0000_0F00_0000#), 24)));
        ret(13) := hexdigits(HexDigitRange(Shift_Right((r and 16#0000_0000_00F0_0000#), 20)));
        ret(14) := hexdigits(HexDigitRange(Shift_Right((r and 16#0000_0000_000F_0000#), 16)));
        ret(15) := hexdigits(HexDigitRange(Shift_Right((r and 16#0000_0000_0000_F000#), 12)));
        ret(16) := hexdigits(HexDigitRange(Shift_Right((r and 16#0000_0000_0000_0F00#), 8)));
        ret(17) := hexdigits(HexDigitRange(Shift_Right((r and 16#0000_0000_0000_00F0#), 4)));
        ret(18) := hexdigits(HexDigitRange(r and 16#0000_0000_0000_000F#));
        return ret;
    end toHexString;

    -- convenience procedure for setting specific bytes in a 64-bit word
    procedure setByte(byte : in byteOffset; val : in Unsigned_8; word : in out Unsigned_64) is
        mask : Unsigned_64 := not (Shift_Left(16#FF#, byte * 8));  -- make a "hole" in the word
        shiftedByte : Unsigned_64;
    begin
        --Ada.Text_IO.Put_Line("setByte mask: " & toHexString(mask));
        --Ada.Text_IO.Put_Line("setByte word in: " & toHexString(word));
        -- zero out that byte
        word := word and mask;

        --Ada.Text_IO.Put_Line("setByte word and mask: " & toHexString(word));
        shiftedByte := Shift_Left(Unsigned_64(val), byte * 8);

        word := word or shiftedByte;
        --Ada.Text_IO.Put_Line("setByte word'Result: " & toHexString(word));
    end setByte;

    procedure setLoWord(val : in Unsigned_32; word : in out Unsigned_64) is
    begin
        word := word and Unsigned_64(16#FFFF_FFFF_0000_0000#);
        word := word or Unsigned_64(val);
    end setLoWord;

    procedure setHiWord(val : in Unsigned_32; word : in out Unsigned_64) is
    begin
        word := word and Unsigned_64(16#0000_0000_FFFF_FFFF#);
        word := word or Shift_Left(Unsigned_64(val), 32);
    end setHiWord;

    function getHiWord(word : in Unsigned_64) return Unsigned_32 is
    begin
        return Unsigned_32(Shift_Right(word, 32));
    end getHiWord;

   ---------------
   -- getLoWord --
   ---------------
   function getLoWord (word : in Unsigned_64) return Unsigned_32 is
   begin
        return Unsigned_32(word and Unsigned_64(16#0000_0000_FFFF_FFFF#));
   end getLoWord;

    -- convenience procedure for getting a specific byte in a 64-bit opcode
    function getByte(byte : in byteOffset; word : in Unsigned_64) return Unsigned_8 is
        mask : Unsigned_64 := Shift_Left(16#FF#, byte * 8);  -- zero everything except our byte
        tmp : Unsigned_64;
    begin
        -- zero out that byte
--        Ada.Text_IO.Put_Line("getByte: word: " & toHexString(word));
--        Ada.Text_IO.Put_Line("getByte: mask: " & toHexString(mask));
        tmp := word and mask;
--        Ada.Text_IO.Put_Line("getByte: word and mask: " & toHexString(tmp));
        tmp := (Shift_Right(tmp, byte * 8));
        return Unsigned_8(tmp);
    end getByte;

end util;
