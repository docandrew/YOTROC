with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with System.Machine_Code;        use System.Machine_Code;
with Ada.Text_IO;                use Ada.Text_IO;

package body vm is

    objectFileLength : Natural := 0;
    
    -- initialize our virtual YOTROC machine. Zeroes memory and loads the program
    -- into memory.
    procedure boot(objectFile : in MachineCodeVector.Vector) is
    begin
        -- start with zeroized registers
        for i in regs'Range loop
            regs(i) := 0;
        end loop;
        
        -- and memory
        for i in memory'Range loop
            memory(i) := 0;
        end loop;
        
        -- and load the program
        for i in 0 .. objectFile.Length - 1 loop
            memory(Integer(i)) := objectFile.Element(Integer(i));
        end loop;
        
        regs(PC) := 0;
        objectFileLength := Natural(objectFile.Length);
    end boot;
    
    procedure dumpRegs is
    begin
        for i in Register'Range loop
            Ada.Text_IO.Put_Line(Register'Image(i) & ": " & regs(i)'Image);
        end loop;
    end dumpRegs;

    -- Execute the next instruction in our program
    function step(msg : out Unbounded_String) return Boolean is
        -- fetch an instruction from memory, and advance the program counter
        -- by 8 bytes. Later, if a jump occurs, the ALU will rewrite it.
        function fetch return Unsigned_64 is
            ret : Unsigned_64;
        begin
            ret := memory(Integer(regs(pc)));
            --Ada.Text_IO.Put_Line("CPU: Fetch " & toHexString(ret));
            return memory(Integer(regs(pc)));
        end fetch;

        -- Given a word of machine code, execute it. This is a combined
        -- Instruction-Decode / Execute / Writeback stage.
        -- Return False if there was a fatal error or if we halt.
        function ID_EX_WB(code : in Unsigned_64) return Boolean is
            type sourceType is (FromRegister, FromMemory, FromImmediate);
            type destType is (ToRegister, ToMemory);

            -- Variant records for keeping track of sources/destinations
            type Source (from : sourceType := FromMemory) is
                record
                    case from is
                       when FromRegister =>
                           sourceReg : Register;
                       when FromMemory =>
                           sourceAddr : Unsigned_64;
                       when FromImmediate =>
                           sourceBits : Unsigned_32;
                    end case;
               end record;

            type Destination (to : destType := ToMemory) is
                record
                   case to is
                       when ToRegister =>
                           destReg : Register;
                       when ToMemory =>
                           destAddr : Unsigned_64;
                   end case;
               end record;

            opcode : Operators;
            loadStoreOperandType : Unsigned_8 := 0;
            offset : Integer;
            source1 : Source;
            source1val : Unsigned_64;
            --source2 : Source;
            --source2val : Unsigned_64;
            dest : Destination;

            -- For arithmetic type operations, we'll use these as indices into
            -- the register file.
            reg1 : Register := Register'Val(getByte(1, code));
            reg2 : Register := Register'Val(getByte(2, code));
            reg3 : Register := Register'Val(getByte(3, code));

            -- For the immediate jump operations, go ahead and get the high bits now.
            jmpLoc : Unsigned_32 := getHiWord(code);
        begin
            -- First, get the operator. It will be the low 6-bits of the instruction.
            opcode := Operators'Val(Integer(code and 16#3F#));

            -- Decode operands for load/store. Other instructions have operands
            -- based on their opcode.
            loadStoreOperandType := Unsigned_8(code and 16#C0#);                    

            -- for loads, dest will be register in byte 1, source will be
            -- dependent on loadStoreOperandType
            if opcode in l8..l64 then
                Ada.Text_IO.Put_Line("CPU decode load instruction: " & toHexString(code));
                dest := (to => ToRegister, destReg => Register'Val(getByte(1,code)));

                case loadStoreOperandType is

                    when loadStoreImmModifier =>
                        source1 := (from => FromImmediate,
                                    sourceBits => getHiWord(code));

                        --regs(dest.destReg) := source1.sourceBits;
                        source1val := Unsigned_64(source1.sourceBits);
                        
                    when loadStoreRegModifier =>
                        -- source register always in byte 2 for loads.
                        source1 := (from => FromRegister,
                                    sourceReg => Register'Val(getByte(2,code)));
                        
                        --regs(dest.destReg) := regs(source.sourceReg);
                        source1val := regs(source1.sourceReg);

                    when loadStoreIndModifier =>
                        source1 := (from => FromMemory,
                                    sourceAddr => regs(Register'Val(getByte(2,code))));
                        
                        --regs(dest.destReg) := memory(source1.sourceAddr);
                        source1val := memory(Integer(source1.sourceAddr));

                    when loadStoreDisModifier =>
                        offset := Integer(getHiWord(code));
                        source1 := (from => FromMemory,
                                    -- OK, this is kind of ugly.
                                    sourceAddr => Unsigned_64(Integer(regs(Register'Val(getByte(2,code)))) + offset));

                        --regs(dest.destReg) := memory(source1.sourceAddr);
                        source1val := memory(Integer(source1.sourceAddr));

                    when others =>
                        msg := To_Unbounded_String("CPU load ERROR: unrecognized operand type. Instruction: " & toHexString(code));
                        return False;
                end case;

                -- now perform the load
                case opcode is
                    when l8 =>
                        -- copy lower 8 bits into register
                        regs(dest.destReg) := regs(dest.destReg) and 16#FFFF_FFFF_FFFF_FF00#;
                        regs(dest.destReg) := regs(dest.destReg) or (16#0000_0000_0000_00FF#
                                               and source1val);
                    when l16 =>
                        -- copy lower 16 bits into register
                        regs(dest.destReg) := regs(dest.destReg) and 16#FFFF_FFFF_FFFF_0000#;
                        regs(dest.destReg) := regs(dest.destReg) or (16#0000_0000_0000_FFFF#
                                              and source1val);
                    when l32 =>
                        -- copy lower 32 bits into register
                        regs(dest.destReg) := regs(dest.destReg) and 16#FFFF_FFFF_0000_0000#;
                        regs(dest.destReg) := regs(dest.destReg) or (16#0000_0000_FFFF_FFFF#
                                              and source1val);
                    when l32u =>
                        -- copy upper 32 bits into register
                        regs(dest.destReg) := regs(dest.destReg) and 16#0000_0000_FFFF_FFFF#;
                        regs(dest.destReg) := regs(dest.destReg) or Shift_Left(source1val, 32);

                    when l64 =>
                        regs(dest.destReg) := source1val;
                    when others =>
                        msg := To_Unbounded_String("CPU load ERROR: unrecognized load opcode. Instruction: " & toHexString(code));
                        return False;
                end case;

                return True;
            end if;

            -- for stores, source will be register in byte 1, dest will be
            -- dependent on loadStoreOperandType
            if opcode in s8..s64 then
                Ada.Text_IO.Put_Line("CPU decode store instruction: " & toHexString(code));
                source1 := (from => FromRegister, sourceReg => Register'Val(getByte(1,code)));

                case loadStoreOperandType is

                    when loadStoreIndModifier =>
                        dest := (to => ToMemory,
                                    destAddr => regs(Register'Val(getByte(2,code))));
                        
                        source1val := regs(source1.sourceReg);

                    when loadStoreDisModifier =>
                        offset := Integer(getHiWord(code));
                        dest := (to => ToMemory,
                                    destAddr => Unsigned_64(Integer(regs(Register'Val(getByte(2,code)))) + offset));

                        source1val := regs(source1.sourceReg);

                    when others =>
                        msg := To_Unbounded_String("CPU store ERROR: illegal instruction: " & toHexString(code));
                        return False;
                end case;

                -- now perform the store
                case opcode is
                    when s8 =>
                        -- copy lower 8 bits into memory
                        memory(Integer(dest.destAddr)) := memory(Integer(dest.destAddr)) and 16#FFFF_FFFF_FFFF_FF00#;
                        memory(Integer(dest.destAddr)) := memory(Integer(dest.destAddr)) or (16#0000_0000_0000_00FF#
                                               and source1val);
                    when s16 =>
                        -- copy lower 16 bits into memory
                        memory(Integer(dest.destAddr)) := memory(Integer(dest.destAddr)) and 16#FFFF_FFFF_FFFF_0000#;
                        memory(Integer(dest.destAddr)) := memory(Integer(dest.destAddr)) or (16#0000_0000_0000_FFFF#
                                              and source1val);
                    when s32 =>
                        -- copy lower 32 bits into memory
                        memory(Integer(dest.destAddr)) := memory(Integer(dest.destAddr)) and 16#FFFF_FFFF_0000_0000#;
                        memory(Integer(dest.destAddr)) := memory(Integer(dest.destAddr)) or (16#0000_0000_FFFF_FFFF#
                                              and source1val);
                    when l64 =>
                        memory(Integer(dest.destAddr)) := source1val;

                    when others =>
                        msg := To_Unbounded_String("CPU store ERROR: unrecognized load opcode. Instruction: " & toHexString(code));
                        return False;
                end case;

                return True;
            end if;
            
            -- Handle other instruction individually
            case opcode is
                when relax =>
                    return True;    -- skip this round
                when avast =>
                    msg := To_Unbounded_String("CPU Execution Halted with avast at address " & toHexString(regs(pc)));
                    return False;   -- halt operation
                when ret =>
                    -- return from function. Shorthand for jump to link reg (r63)
                    regs(pc) := regs(r63);
                when btc =>
                    -- clear a bit
                    declare
                        curVal : Unsigned_64 := regs(reg1);
                        mask : Unsigned_64 := not Shift_Left(1, Integer(getByte(2, code)));
                    begin
                        regs(reg1) := curVal and mask;
                    end;
                when bts =>
                    -- set a bit
                    declare
                        curVal : Unsigned_64 := regs(reg1);
                        mask : Unsigned_64 := Shift_Left(1, Integer(getByte(2, code)));
                    begin
                        regs(reg1) := curVal or mask;
                    end;
                when tb =>
                    -- test whether a bit is set or not
                    declare
                        curVal : Unsigned_64 := regs(reg1);
                        mask : Unsigned_64 := Shift_Left(1, Integer(getByte(2, code)));
                    begin
                        if (curVal and mask) = 0 then
                            flags.zero := True;
                        else
                            flags.zero := False;
                        end if;
                    end;
                -- Arithmetic operations. Note we don't check for overflow, sign
                --   bits, or make sure that the appropriate registers are being used.
                when add =>
                    regs(reg1) := regs(reg2) + regs(reg3);
                when fadd =>
                    regs(reg1) := rawLongFloatBits(toDouble(regs(reg2)) + toDouble(regs(reg3)));
                when sub =>
                    regs(reg1) := regs(reg2) - regs(reg3);
                when fsub =>
                    regs(reg1) := rawLongFloatBits(toDouble(regs(reg2)) - toDouble(regs(reg3)));
                when mul =>
                    regs(reg1) := regs(reg2) * regs(reg3);
                when fmul =>
                    regs(reg1) := rawLongFloatBits(toDouble(regs(reg2)) * toDouble(regs(reg3)));
                when div =>
                    regs(reg1) := regs(reg2) / regs(reg3);
                when fdiv =>
                    regs(reg1) := rawLongFloatBits(toDouble(regs(reg2)) / toDouble(regs(reg3)));

                -- bitwise operations
                when modb =>
                    regs(reg1) := regs(reg2) mod regs(reg3);
                when orb =>
                    regs(reg1) := regs(reg2) or regs(reg3);
                when andb =>
                    regs(reg1) := regs(reg2) and regs(reg3);
                when xorb =>
                    regs(reg1) := regs(reg2) xor regs(reg3);
                when int =>
                    compoundInterest : declare
                        package Exponentiation is new
                            Ada.Numerics.Generic_Elementary_Functions(Long_Float);
                        use Exponentiation;
                        interestRate : Long_Float := Long_Float(toDouble(regs(reg2)));
                        principal : Long_Float := Long_Float(toDouble(regs(reg1)));
                        years : Long_Float := Long_Float(toDouble(regs(reg3)));
                        result : Long_Float;
                    begin
                        result := principal * (1.0 + interestRate)**years;
                        Ada.Text_IO.Put_Line("int result: " & result'Image);
                        regs(reg1) := rawLongFloatBits(result);
                    end compoundInterest;

                when knots =>
                    regs(reg1) := rawLongFloatBits(toDouble(regs(reg2)) * 0.869);
                when miles =>
                    regs(reg1) := rawLongFloatBits(toDouble(regs(reg2)) * 1.151);

                when itd =>
                    regs(reg1) := rawLongFloatBits(Long_Float(regs(reg2)));

                when std =>
                    regs(reg1) := rawLongFloatBits(Long_Float(fromFloatImmediate(util.getLoWord(regs(reg1)))));

                when cb =>
                    -- count set bits in this register
                    declare
                        tmp : Unsigned_64;
                        count : Unsigned_64;
                    begin
                        tmp := regs(reg2);
                        Asm("popcnt %1, %0", Inputs => Unsigned_64'Asm_Input("r", tmp),
                                         Outputs => Unsigned_64'Asm_Output("=r", count));
                        regs(reg1) := count;
                    end;
                when shll =>
                    regs(reg1) := Shift_Left(regs(reg1), Integer(regs(reg2)));
                when shrl =>
                    regs(reg1) := Shift_Right(regs(reg1), Integer(regs(reg2)));
                when cmp =>
                    if regs(reg1) > regs(reg2) then
                        flags.gt := True;
                        flags.lt := False;
                        flags.eq := False;
                    elsif regs(reg1) < regs(reg2) then
                        flags.gt := False;
                        flags.lt := True;
                        flags.eq := False;
                    else
                        flags.gt := False;
                        flags.lt := False;
                        flags.eq := True;
                    end if;

                when notb =>
                    regs(reg1) := not regs(reg1);

                -- Branches
                when jmp =>
                    -- need to be careful about off-by-8 errors here.
                    regs(pc) := regs(reg1);
                when jz =>
                    if flags.zero then
                        regs(pc) := regs(reg1);
                    end if;
                when jeq =>
                    if flags.eq then
                        regs(pc) := regs(reg1);
                    end if;
                when jne =>
                    if not flags.eq then
                        regs(pc) := regs(reg1);
                    end if;
                when jlt =>
                    if flags.lt then
                        regs(pc) := regs(reg1);
                    end if;
                when jgt =>
                    if flags.gt then
                        regs(pc) := regs(reg1);
                    end if;
                when call =>
                    regs(r63) := regs(pc);  -- store addr of next instruction in link reg.
                                            -- PC has already been advanced above.
                    regs(pc) := regs(reg1);

                when jmpa =>
                    regs(pc) := Unsigned_64(jmpLoc);
                when jza =>
                    if flags.zero then
                        regs(pc) := Unsigned_64(jmpLoc);
                    end if;
                when jeqa =>
                    if flags.eq then
                        regs(pc) := Unsigned_64(jmpLoc);
                    end if;
                when jnea =>
                    if not flags.eq then
                        regs(pc) := Unsigned_64(jmpLoc);
                    end if;
                when jlta =>
                    if flags.lt then
                        regs(pc) := Unsigned_64(jmpLoc);
                    end if;
                when jgta =>
                    if flags.gt then
                        regs(pc) := Unsigned_64(jmpLoc);
                    end if;
                when others =>
                    msg := To_Unbounded_String("CPU Execute ERROR: illegal instruction " & toHexString(code));
                    return False;
            end case;

            -- If we did an arithmetic-ish operation, if it results in the first 
            -- reg being zero, then go ahead and set the zero flag.
            if opcode in btc .. notb then
                if regs(reg1) = 0 then
                    flags.zero := True;                    
                end if;
            end if;

            return True;
        end ID_EX_WB;

        code : Unsigned_64;
    begin
        -- set zero register before each instruction so writes won't matter
        regs(z) := 0;
        code := fetch;
        Ada.Text_IO.Put_Line("CPU fetched: instruction " & toHexString(code) & " from addr " & toHexString(regs(pc)));
        -- advance the PC after we fetch the instruction. If there are any jumps
        -- in the ALU, they will change it.
        regs(pc) := regs(pc) + 1;
        return ID_EX_WB(code);
    end step;
    
    -- Keep running the program.
    --procedure execute is
    --begin
--          flags.zero := False;
--          flags.lt := False;
--          flags.gt := False;
--          flags.eq := False;
--          flags.overflow := False;
--          flags.sign := False;
--          flags.carry := False;
--  
--          InstructionLoop : loop
--              if not step then
--                   return;
--              end if;
--              -- delay here a half-second between instructions for 2 reasons: 
--              -- 1, if we put an infinite loop
--              -- in the code it is going to slow down our GUI a bunch, and 2,
--              -- it lets us follow along as the code runs.
--              delay 0.5;
--              -- this is just the fetch-decode-execution cycle, if there's a loop
--              -- in the program itself, it will continue to execute
--              exit InstructionLoop when regs(pc) > Unsigned_64(objectFileLength);
--          end loop InstructionLoop;
--      end execute;

end vm;
