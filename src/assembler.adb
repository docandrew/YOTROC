
-- BNF(ish) grammar for our parser. Assume all production rules are
-- newline-terminated.
--
-- Reserved words: registers, "Define"
--
--  <comment> := ";", string
--  <label> := "@", {alphanum}
--
--  //Identifier must start with a letter, then any sequence of letters
--  // and numbers. Can not be a reserved word.
--  <identifier> := [alpha], {alphanum}
--
--  <directive> := "%", <dirname>, <identifier>, <real>
--
--  //For instructions:
--  //Our parser will just keep eating operands until we either get 3 or
--  //we reach LF or EOF. Later, during codegen, we will
--  // do semantic analysis to see if they make sense for the instruction.
--  <instruction> := <operator>, {<operand>} //up to 3 operands.
--
--  <operand> := <register> | <immediate> | <indirect> | <indexed>
--   <register> := ('R' | 'F'),natural | 'Z'
--   <immediate> := <integer> | <float> | <identifier>
--   <indirect> := '*',
--   <indexed> := '*(', <register>, '+', <natural>, ')'
--
--  <real> := <integer> | <natural>
--  <integer> := "0" | ['-'], <natural>
--  <natural> := {digit}
--  <float> := <integer>, '.', <natural>
--  
--
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers.Hashed_Maps;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with util; use util;
with vm; use vm;

package body assembler is
        
    pos : Natural := 0;

    -- Hashmap to store identifiers with their values
    function Equivalent_Keys(Left : in Unbounded_String; Right : in Unbounded_String) return Boolean is
    begin
        return Left = Right;
    end Equivalent_Keys;

    function Hash_Func(Key : in Unbounded_String) return Ada.Containers.Hash_Type is
    begin
        return Ada.Strings.Hash(Ada.Strings.Unbounded.To_String(Key));
    end Hash_Func;

    -- An identifier is just a variable representing some kind of constant (YOTROCReal).
    package Identifier_Map is new Ada.Containers.Hashed_Maps(
        Key_Type => Unbounded_String,
        Element_Type => YOTROCReal,
        Hash => Hash_Func,
        Equivalent_Keys => Equivalent_Keys);

    -- A label is just a name fixed at a certain line.
    --package Label_Map is new Ada.Containers.Hashed_Maps(
    --    Key_Type => Unbounded_String,
    --    Element_Type => Natural,
    --    Hash => Hash_Func,
    --    Equivalent_Keys => Equivalent_Keys);
    
    -- globals for identifiers and labels.
    -- after a parse run, these need to be present for codegen.
    identifiers : Identifier_Map.Map;
    --labels : Label_Map.Map;

    ---------------------------------------------------------------------------
    -- Procedures for debugging, printing contents of the label and identifier maps.
    -- probably a nicer way to use generics here
    ---------------------------------------------------------------------------
    --procedure printLabelEntry(position : Label_Map.Cursor) is
    --begin
    --    Ada.Text_IO.Put_Line(" Label: " & To_String(Label_Map.Key(position)) & 
    --                         " Address: " & Natural'Image(Label_Map.Element(position)));
    --end printLabelEntry;

    procedure printIdentifierEntry(position : Identifier_Map.Cursor) is
        element : YOTROCReal;
        elemStr : Unbounded_String;
    begin
        element := Identifier_Map.Element(position);

        if(element.kind = IntegerReal) then
            elemStr := To_Unbounded_String(Integer'Image(Integer(element.realInt)));
        else
            elemStr := To_Unbounded_String(Float'Image(Float(element.realFloat)));
        end if;

        Ada.Text_IO.Put_Line(" Identifier: " & To_String(Identifier_Map.Key(position)) & 
                             " Value: " & To_String(elemStr));
    end printIdentifierEntry;

    --procedure dumpLabels is
    --begin
    --    labels.Iterate(printLabelEntry'Access);
    --end dumpLabels;
    
    procedure dumpIdentifiers is
    begin
        identifiers.Iterate(printIdentifierEntry'Access);
    end dumpIdentifiers;

    ---------------------------------------------------------------------------
    -- parse a YOTROC .asm file
    -- Param source - valid .asm file
    -- Param out instructions - if successful in parsing, output a vector of 
    --  Instruction records
    -- Param out msg - if unsuccessful, this will be a String containing the 
    --  error message and offending line.
    --
    -- We don't care if the operand types make sense for the instruction at this 
    --  point.
    -- During codegen, we'll do semantic analysis to sanity-check them.
    ---------------------------------------------------------------------------
    function parse(source : in String; instructions : out InstructionVector.Vector; msg : out Unbounded_String) return Boolean is
        use ASCII;

        -- to track where in the file we are for parsing
        --nextChar : Character;
        pos : Integer := 0;

        -- track what line of file we are in for error messages
        curLine : Integer := 1;

        -- every instruction advances this by 8 bytes. We use this to keep track of what address
        -- a label is at.
        curAddress : Natural := 0;

        -- Advance the cursor and return the char at this cursor
        -- If we reach the EOF, return NUL
        function getNext return Character is
        begin

            pos := pos + 1;
            
            if pos > source'Last then
                return NUL;
            else
                if source(pos) = LF then
                    curLine := curLine + 1;
                end if;

                --Ada.Text_IO.Put_Line(pos'Image & " " & source(pos));

                return source(pos);
            end if;
        end getNext;

        -- Look at the next character without advancing cursor
        function peekNext return Character is
        begin
            if pos < source'Last then
                return source(pos + 1);
            else
                return NUL;
            end if;
        end peekNext;

        -- identify whitespace
        function isWhite(c : Character) return Boolean is
        begin
            if c = ' ' or c = HT then
                return True;
            else
                return False;
            end if;
        end isWhite;

        -- identify the end of a word
        function isWordEnd(c : Character) return Boolean is
        begin
            if isWhite(c) or c = ';' or c = ')' or c = '+' or c = NUL or c = LF then
                return True;
            else
                return False;
            end if;
        end isWordEnd;

        -- skip whitespace
        procedure skipWhite is
            ignore : Character;
        begin
            while isWhite(peekNext) loop
                ignore := getNext;
            end loop;
        end skipWhite;

        -- skip comment until end of line.
        procedure skipComment is
            ignore : Character;
        begin
            Ada.Text_IO.Put("PARSE: Line" & curLine'Image & " Skipping Comment ");

            while peekNext /= LF and then peekNext /= NUL loop
                ignore := getNext;
                Ada.Text_IO.Put(ignore);
            end loop;

            Ada.Text_IO.Put_Line("");
        end skipComment;

        -- determine whether this identifier is valid or not.
        function isValidIdentifier(ubstr : Unbounded_String) return Boolean is
            str : String := To_String(ubstr);
        begin
            if not Is_Letter(str(1)) then
                msg := To_Unbounded_String("Error, line" & curLine'Image & ", Identifier must start with a letter");
                return False;
            end if;

            for chr of str loop
                if not (Is_Alphanumeric(chr) or chr = '_') then
                    msg := To_Unbounded_String("Error, line" & curLine'Image & ", Identifers can only contain letters, numbers or underscores.");
                    return False;
                end if;
            end loop;
            return True;
        end isValidIdentifier;

        -- Get a single word, like "define" or "R12"
        function getName(dir : out Unbounded_String) return Boolean is
            chr : Character;
        begin
            while not isWordEnd(peekNext) loop
                chr := getNext;
                Append(dir, To_Lower(chr));
            end loop;

            return True;
        end getName;

        -- all identifiers must start with a letter and then be alpha-numeric or underscore
        function getIdentifier(ident : out Identifier) return Boolean is
            identStr : Unbounded_String;
            --chr : Character;
            success : Boolean;
        begin
            Ada.Text_IO.Put("PARSE: Line" & curLine'Image & " Identifier: ");
            success := getName(identStr);

            if not isValidIdentifier(identStr) then
                return False;
            end if;

            --TODO: check against list of reserved words?

            Ada.Text_IO.Unbounded_IO.Put_Line(identStr);
            ident := Identifier(identStr);
            return True;
        end getIdentifier;

        -- parse a label. It works just like an identifier but using the label's
        -- address instead of a predefined value.
        function doLabel return Boolean is
            labelValue : YOTROCReal;
            identStr : Unbounded_String;
        begin
            -- read the label, if good, add it to the identifier hashmap with current address.
            if not getIdentifier(identStr) then
                return False;
            else
                -- TODO: check to make sure we aren't clobbering another identifier.
                labelValue := (kind => IntegerReal, realInt => curAddress);
                identifiers.Insert(Key => identStr, New_Item => labelValue);
                
                Ada.Text_IO.Put("PARSE: Line" & curLine'Image & " Label: ");
                Ada.Text_IO.Unbounded_IO.Put(identStr);
                Ada.Text_IO.Put_Line(" at address " & curAddress'Image);
                return True;
            end if;
        end doLabel;

        -- check to see if a word is a number or not.
        function isReal(ubstr : in Unbounded_String; num : out YOTROCReal) return Boolean is
            isFloat : Boolean := False;
            str : String := To_String(ubstr);
            numFloat : Float;
            numInteger : Integer;
        begin
            -- first check to see if this is a float or not, so we know how to store it.
            for c of str loop
                if c = '.' then
                    isFloat := True;
                end if;
            end loop;

            if isFloat then
                numFloat := Float'Value(str);
                num := (kind => FloatReal, realFloat => numFloat);
            else
                numInteger := Integer'Value(str);
                num := (kind => IntegerReal, realInt => numInteger);
            end if;
            
            return True;

        exception   -- problem parsing the number.
            when others =>
                return False;
        end isReal;

        -- parse a real number
        function getReal(num : out YOTROCReal) return Boolean is
            --chr : Character;
            asStr : Unbounded_String;
            isFloat : Boolean := False;
            --numInteger : Long_Integer;
            --numFloat : Float;
        begin
            while not isWordEnd(peekNext) loop
                Append(asStr, getNext);
            end loop;

            Ada.Text_IO.Put_Line("PARSE: Line" & curLine'Image & " Literal: " & To_String(asStr));

            if not isReal(asStr, num) then
                msg := To_Unbounded_String("Error, line" & curLine'Image & ", invalid number literal " & To_String(asStr));
                return False;
            end if;

            return True;
        end getReal;

        -- parse a directive. A directive is the directive itself, then some extra stuff like
        -- an identifier or an immediate.
        function doDirective return Boolean is
            directive : Unbounded_String;
            ident : Identifier;
            value : YOTROCReal;
            --chr : Character;
            success : Boolean := False;
        begin
            Ada.Text_IO.Put("PARSE: Line" & curLine'Image & " Directive %");

            -- Get the directive name
            success := getName(directive);
            Ada.Text_IO.Unbounded_IO.Put_Line(directive);

            if not success then
                return False;
            end if;

            -- only support "define" right now.
            if directive = "define" then
                skipWhite;
                success := getIdentifier(ident);

                if not success then
                    return False;
                else
                    skipWhite;
                    success := getReal(value);
                    
                    if success then
                        -- cool, we defined an identifier.
                        identifiers.Insert(Key => ident, New_Item => value);
                    else
                        return False;
                    end if;
                end if;
            else
                msg := To_Unbounded_String("Error, line" & curLine'Image & ", Unsupported assembler directive: " & To_String(directive));
                return False;
            end if;

            -- other interesting directives might be:
            -- %address LITERAL : set address the "object file" gets loaded at

            return True;
        end doDirective;

        -- check to see if a given word is in our list of registers. Return True
        -- and set the Register value if this is in fact a register.
        function isRegister(str : in Unbounded_String; reg : out Register) return Boolean is
        begin
            for r in Register loop
                --Ada.Text_IO.Put_Line(" checking" & Register'Image(r) & " and " & To_String(str));
                if Register'Image(r) = To_Upper(To_String(str)) then
                    reg := r;
                    return True;
                end if;
            end loop;

            return False;
        end isRegister;

        -- get a register
        function getRegister(reg : out Register) return Boolean is
            curWord : Unbounded_String;
            --success : Boolean;
            goodRegister : Boolean := False;
        begin
            Ada.Text_IO.Put_Line("PARSE: Line" & curLine'Image & " Register");

            if not getName(curWord) then
                msg := To_Unbounded_String("Error, line" & curLine'Image & ", expected register name");
                return False;
            end if;

            -- capitalize it so we can compare with enumerated type
            curWord := To_Unbounded_String(To_Upper(To_String(curWord)));
            goodRegister := isRegister(curWord, reg);

            if not goodRegister then
                msg := To_Unbounded_String("Error, line" & curLine'Image & ", Unrecognized register: " & To_String(curWord));
                return False;
            end if;

            Ada.Text_IO.Put("PARSE: Line" & curLine'Image & " Register: ");
            Ada.Text_IO.Put_Line(Register'Image(reg));

            return True;
        end getRegister;

        -- get a single operand. This is kind of a mess, just because of how
        -- many different operands we support, and I don't like prefixing 
        -- registers or immediates with a special character,
        -- because we can figure it out here.
        function getOperand(op : out Operand) return Boolean is
            curWord : Unbounded_String;
            reg : Register;
            chr1 : Character;
            chr2 : Character;
            offset : YOTROCReal;
            immedNumber : YOTROCReal;
            immed : Immediate;
            success : Boolean;
        begin
            Ada.Text_IO.Put("PARSE: Line" & curLine'Image & " Operand ");

            chr1 := peekNext;
            if(chr1 = '*') then
                -- either an offset or register indirect
                chr1 := getNext;  --eat the *
                chr2 := peekNext; --check next char

                if chr2 = '(' then
                    chr2 := getNext;  --eat the (
                    -- displacement, first word must be a register
                    success := getRegister(reg);
                    if not success then
                        return False;
                    end if;

                    skipWhite;
                    -- expect a plus sign
                    chr1 := getNext;
                    if chr1 /= '+' then
                        msg := To_Unbounded_String("Error, line" & curLine'Image & 
                         ", invalid syntax for displacement operand. (should be *(Reg + Offset))");
                        return False;
                    end if;
                            
                    skipWhite;
                    -- should be a number here, must be an Integer. Would be cool
                    -- to be able to use identifiers here too, but, eh.
                    success := getReal(offset);
                    if (not success) or (offset.kind = FloatReal) then
                        msg := To_Unbounded_String("Error, line" & curLine'Image &
                         ", invalid offset for displacement operand. (should be *(Reg + <integer>))");
                        return False;
                    end if;

                    skipWhite;
                    chr1 := getNext;
                    if chr1 /= ')' then
                        msg := To_Unbounded_String("Error, line" & curLine'Image &
                         ", missing closing parenthesis on displacement operand.");
                        return False;
                    end if;

                    Ada.Text_IO.Put_Line("Displacement w/ offset");
                    op := (kind => Displacement, regBase => reg, offset => offset.realInt);
                    return True;
                else
                    -- register indirect. Should be a General-Purpose reg, but
                    -- we'll check for that in code gen.
                    success := getRegister(reg);
                    if not success then
                        return False;
                    end if;

                    Ada.Text_IO.Put_Line("Displacement, no offset (reg indirect)");
                    op := (kind => Displacement, regBase => reg, offset => 0);
                    return True;
                end if;                               
            else
                -- don't know what type of operand this is, so just get it as
                -- a string for now, then check it.
                success := getName(curWord);

                -- Try register.
                if isRegister(curWord, reg) then
                    Ada.Text_IO.Put_Line("Register");
                    op := (kind => RegisterOperand, regName => reg);
                    return True;
                end if;

                -- try and parse as a number. This is kind of a mess.
                if isReal(curWord, immedNumber) then
                    Ada.Text_IO.Put_Line("Immediate");
                    if immedNumber.kind = IntegerReal then
                        immed := (kind => IntegerImm, immInt => immedNumber.realInt);
                    else
                        immed := (kind => FloatImm, immFloat => immedNumber.realFloat);
                    end if;

                    op := (kind => ImmediateOperand, imm => immed);
                    return True;                      
                end if;

                -- Has to be an identifier. We don't bother checking to see if
                -- the identifier exists here, we'll do that in code gen.
                -- This way, we have the ability to jump to labels that are
                -- declared later in the code.
                if isValidIdentifier(curWord) then
                    Ada.Text_IO.Put_Line("Immediate (identifier)");
                    immed := (kind => IdentifierImm, immID => curWord);
                    op := (kind => ImmediateOperand, imm => immed);
                    return True;
                end if;
                
                msg := To_Unbounded_String("Error, line" & curLine'Image &
                         ", invalid operand. (Must be immediate, register, displacement or an identifier)");
                return False;
            end if;
        end getOperand;

        -- get an Operator (basic instruction with no operands)
        function getOperator(operator : out Operators) return Boolean is
            success : Boolean;
            goodOperator : Boolean;  -- if this operator is in our list defined in vm.ads
            curWord : Unbounded_String;
        begin
            success := getName(curWord);
            curWord := To_Unbounded_String(To_Upper(To_String(curWord)));

            Ada.Text_IO.Put("PARSE: Line" & curLine'Image & " Operator: ");

            -- instruction list.
            -- make sure this operator is in our list
            for op in Operators loop
                --Ada.Text_IO.Put_Line(" checking match with instruction " & Operators'Image(op) & ".");
                if Operators'Image(op) = To_String(curWord) then
                    operator := op;
                    Ada.Text_IO.Put_Line(Operators'Image(operator));
                    goodOperator := True;
                    exit;
                end if;
            end loop;

            if not goodOperator then
                msg := To_Unbounded_String("Error, line" & curLine'Image & ", Unrecognized instruction: " & To_String(curWord));
                return False;
            end if;

            return True;
        end getOperator;

        -- attempt to parse an instruction
        function doInstruction return Boolean is
            inst : Instruction;
            operator : Operators;
            type OperandArr is array (0..2) of Operand;
            operands : OperandArr;
            operandCount : Integer;
            success : Boolean;
            curWord : Unbounded_String;
            goodOperator : Boolean := False;
        begin
            Ada.Text_IO.Put_Line("PARSE: Line" & curLine'Image & " Instruction");
            -- Get the instruction. It should be something of type Operators
            success := getOperator(operator);
            if not success then
                return False;
            end if;

            skipWhite;
            operandCount := 0;
                
            -- now start eating operands until the end of the line.
            while peekNext /= LF and peekNext /= ';' and peekNext /= NUL loop
                success := getOperand(operands(operandCount));
                if not success then
                    return False;
                else
                    skipWhite;
                    operandCount := operandCount + 1;
                end if;
            end loop;

            case operandCount is
                when 0 =>
                    inst := (kind => NoOperand, operator => operator, line => curLine);
                when 1 =>
                    inst := (kind => OneOperand, operator => operator, line => curLine, op1 => operands(0));
                when 2 =>
                    inst := (kind => TwoOperand, operator => operator, line => curLine, op1 => operands(0), 
                             op2 => operands(1));
                when 3 =>
                    inst := (kind => ThreeOperand, operator => operator, line => curLine, op1 => operands(0), 
                             op2 => operands(1), op3 => operands(2));
                when others =>
                    msg := To_Unbounded_String("Error, line" & curLine'Image & ", Too many operands for instruction. (Max 3)");
            end case;
            
            -- add the instruction to our list.
            instructions.Append(New_Item => inst);
            -- Advance the current address by 8 bytes.
            curAddress := curAddress + 1;
                                     
            return True;
        end doInstruction;

    -- begin the parse() function itself
        ignore : Character;
    begin
        -- in case we are doing successive parse() runs (like for testing)
        identifiers.Clear;
        --labels.Clear;
        
        while pos < source'Last loop
            skipWhite;
            case peekNext is
                when NUL =>
                    -- Reached the end of the file. If no errors by this point,
                    -- we were successful.
                    exit;
                when ';' =>
--                    ignore := getNext; -- eat the ;
                    skipComment;
                when '@' =>
                    ignore := getNext; -- eat the @
                    if not doLabel then
                        return False;
                    end if;
                when '%' =>
                    ignore := getNext; -- eat the %
                    if not doDirective then
                        return False;
                    end if;
                when LF =>
                    ignore := getNext;
                    null;
                when others =>
                    if not doInstruction then
                        return False;
                    end if;
            end case;
        end loop;

        msg := To_Unbounded_String("Success");
        return True;
    end parse;

    -- Given a vector of instructions, sanity-check to make sure operands match
    -- the operators, look up identifiers in the table and fill in their values,
    -- and emit machine code for each instruction.
    function codeGen(instructions : in InstructionVector.Vector;
                     objectFile : out MachineCodeVector.Vector;
                     msg : out Unbounded_String) return Boolean is

        --success : Boolean;
        opcode : Unsigned_64;

        -- Try and get an integer value from an immediate. This function will perform
        -- a lookup in the identifier table if it refers to one.
        function getImmediateIntegerValue(imm : in Immediate; int : out Integer) return Boolean is
            immedReal : YOTROCReal;
        begin
            if imm.kind = IdentifierImm then
            -- lookup identifier if it is.
                lookupIdentifier : declare
                begin
                    immedReal := identifiers.Element(Key => imm.immID);
                exception
                    when others =>
                        return False;
                end lookupIdentifier;

                if(immedReal.kind /= IntegerReal) then
                -- this identifier maps to a float
                    --msg := To_Unbounded_String("Error, identifier points to floating-point type for " & inst.operator'Image);
                    return False;
                end if;

                int := immedReal.realInt;
                return True;
            elsif imm.kind = IntegerImm then
            -- otherwise just take the immediate as is.
                int := imm.immInt;
                return True;
            else
            -- this was a raw float immediate
                --msg := To_Unbounded_String("Error, identifier points to floating-point type for " & inst.operator'Image);
                return False;
            end if;
        end getImmediateIntegerValue;

        -- Try and get a float value from an immediate. This function will perform
        -- a lookup in the identifier table if it refers to one.
        function getImmediateFloatValue(imm : in Immediate; flt : out Float) return Boolean is
            immedReal : YOTROCReal;
        begin
            if imm.kind = IdentifierImm then
            -- lookup identifier if it is.
                lookupIdentifier : declare
                begin
                    immedReal := identifiers.Element(Key => imm.immID);
                exception
                    when others =>
                        return False;
                end lookupIdentifier;

                if(immedReal.kind /= FloatReal) then
                -- this identifier maps to an integer
                    --msg := To_Unbounded_String("Error, identifier points to integer type for " & inst.operator'Image);
                    return False;
                end if;

                flt := immedReal.realFloat;
                return True;
            elsif imm.kind = FloatImm then
            -- otherwise just take the immediate as is.
                flt := imm.immFloat;
                return True;
            else
            -- this immediate was a raw integer
                --msg := To_Unbounded_String("Error, identifier points to integer type for " & inst.operator'Image);
                return False;
            end if;
        end getImmediateFloatValue;

        -- Given an Instruction, generate a single word of machine code
        function genCode(inst : in Instruction; 
                           code : out Unsigned_64; 
                           msg : out Unbounded_String) return Boolean is

            --immedReal : YOTROCReal; -- for identifier -> real number lookups.
            immedInt : Integer;     -- for holding integer immediates
            immedFloat : Float;     -- for holding float immediates


            modifiedOpcode : Integer := 0;
        begin
            -- initialize opcode as all zeroes to start
            code := 0;

            -- determine the instruction type, see vm.ads for list
            case inst.operator is
                -- load/store operation. Need to gen opcode based on operands, so there
                -- is some extra work here to ensure operands match the instruction
                when ABCDOps =>
                    if inst.kind /= TwoOperand then
                        msg := To_Unbounded_String("Error, line" & inst.line'Image & ": expected two operands for " & inst.operator'Image);
                        return False;
                    end if;

                    -- first, handle loads. Loads can be reg reg/reg imm/reg disp
                    if inst.operator in l8 .. l64 then
                        -- first operand must be a register
                        if inst.op1.kind /= RegisterOperand then
                            msg := To_Unbounded_String("Error, line" & inst.line'Image & ": first operand must be register for " & inst.operator'Image);
                            return False;
                        end if;
                        -- second operand can be any type.
                    end if;

                    -- now, stores. Stores can be reg imm/reg disp
                    if inst.operator in s8 .. s64 then
                        -- first operand must be a register
                        if inst.op1.kind /= RegisterOperand then
                            msg := To_Unbounded_String("Error, line" & inst.line'Image & ": first operand must be register for " & inst.operator'Image);
                            return False;
                        end if;

                        if inst.op2.kind = RegisterOperand or inst.op2.kind = ImmediateOperand then
                            msg := To_Unbounded_String("Error, line" & inst.line'Image & ": second operand must be register indirect or displacement for " & inst.operator'Image);
                            return False;
                        end if;
                    end if;

                    -- Determine what modifier to use based on the second operand.
                    case inst.op2.kind is

                        when ImmediateOperand =>
                            modifiedOpcode := Integer(Operators'Pos(inst.operator)) + loadStoreImmModifier;
                            -- get the immediate here too. Try and get it as an integer first, if that
                            -- fails then it must be a float.
                            if getImmediateIntegerValue(inst.op2.imm, immedInt) then
                                setHiWord(Unsigned_32(immedInt), code);    -- put the immediate in machine code

                                -- Only floating point immediates allowed in FP regs.
                                if inst.op1.regName in FloatRegister then
                                    msg := To_Unbounded_String("Error, line" & inst.line'Image & ": cannot assign an integer immediate to the FP Register " & inst.operator'Image);
                                    return False;
                                end if;

                            elsif getImmediateFloatValue(inst.op2.imm, immedFloat) then
                                setHiWord(rawFloatBits(immedFloat), code);  -- put the immediate in machine code

                                -- do a quick sanity check here. If we are trying to put a float immediate in a
                                -- GPR, error.
                                if inst.op1.regName not in FloatRegister then
                                    msg := To_Unbounded_String("Error, line" & inst.line'Image & ": cannot assign a floating immediate to the GPR " & inst.operator'Image);
                                    return False;
                                end if;
                            else
                                msg := To_Unbounded_String("Error, line" & inst.line'Image & ": undefined immediate " & To_String(inst.op2.imm.immID));
                                return False;
                            end if;

                        when RegisterOperand =>
                            modifiedOpcode := Integer(Operators'Pos(inst.operator)) + loadStoreRegModifier;

                        when Displacement =>
                            -- we use the same section of the Operand variant record for register indirect
                            -- and displacement, but the opcodes are different, so we discriminate between
                            -- the two by checking the offset.
                            if inst.op2.offset = 0 then
                                modifiedOpcode := Integer(Operators'Pos(inst.operator)) + loadStoreIndModifier;
                            else
                                modifiedOpcode := Integer(Operators'Pos(inst.operator)) + loadStoreDisModifier;
                                setHiWord(Unsigned_32(inst.op2.offset), code);  -- put displacement in machine code
                            end if;

                        when others =>
                            msg := To_Unbounded_String("Error, line" & inst.line'Image & ": unrecognized addressing type for " & inst.operator'Image);
                            return False;
                    end case;

                    -- if there was an immediate or displacement required, they are already set above.
                    setByte(0, Unsigned_8(modifiedOpcode), code);
                    setByte(1, Register'Pos(inst.op1.regName), code);

                    -- if this isn't an immediate, we need to set Reg 2
                    if inst.op2.kind = RegisterOperand then
                        setByte(2, Register'Pos(inst.op2.regName), code);
                    elsif inst.op2.kind = Displacement then
                        setByte(2, Register'Pos(inst.op2.regBase), code);
                    end if;

                    Ada.Text_IO.Put_Line("CODEGEN: emitting ABCD-Type code " & toHexString(code));
                    return True;
                -- Three-operand arithmetic. All of these should be registers.
                when ETypeOps =>
                    if inst.kind /= ThreeOperand then
                        msg := To_Unbounded_String("Error, line" & inst.line'Image & ": expected three operands for " & inst.operator'Image);
                        return False;
                    end if;

                    if inst.op1.kind /= RegisterOperand or inst.op2.kind /= RegisterOperand or
                       inst.op3.kind /= RegisterOperand then
                        msg := To_Unbounded_String("Error, line" & inst.line'Image & ": expected three registers for " & inst.operator'Image);
                        return False;
                    end if;

                    setByte(0, Unsigned_8(Operators'Pos(inst.operator)), code);
                    setByte(1, Unsigned_8(Register'Pos(inst.op1.regName)), code);
                    setByte(2, Unsigned_8(Register'Pos(inst.op2.regName)), code);
                    setByte(3, Unsigned_8(Register'Pos(inst.op3.regName)), code);
                    Ada.Text_IO.Put_Line("CODEGEN: emitting E-Type code " & toHexString(code));
                    return True;
                -- Two-operand arithmetic. Both of these should be registers.
                when FTypeOps =>
                    if inst.kind /= TwoOperand then
                        msg := To_Unbounded_String("Error, line" & inst.line'Image & ": expected two operands for " & inst.operator'Image);
                        return False;
                    end if;

                    if inst.op1.kind /= RegisterOperand or inst.op2.kind /= RegisterOperand then
                        msg := To_Unbounded_String("Error, line" & inst.line'Image & ": expected two registers for " & inst.operator'Image);
                        return False;
                    end if;

                    setByte(0, Unsigned_8(Operators'Pos(inst.operator)), code);
                    setByte(1, Unsigned_8(Register'Pos(inst.op1.regName)), code);
                    setByte(2, Unsigned_8(Register'Pos(inst.op2.regName)), code);
                    Ada.Text_IO.Put_Line("CODEGEN: emitting F-Type code " & toHexString(code));
                    return True;

                -- Bit operation, expect register, immediate.
                when GTypeOps =>
                    if inst.kind /= TwoOperand then
                        msg := To_Unbounded_String("Error, line" & inst.line'Image & ": expected two operands for " & inst.operator'Image);
                        return False;
                    end if;

                    if inst.op1.kind /= RegisterOperand then
                        msg := To_Unbounded_String("Error, line" & inst.line'Image & ": expected register as first operand for " & inst.operator'Image);
                        return False;
                    end if;

                    if inst.op2.kind /= ImmediateOperand or inst.op2.imm.kind /= IntegerImm then
                        msg := To_Unbounded_String("Error, line" & inst.line'Image & ": expected immediate integer type as second operand for " & inst.operator'Image);
                        return False;
                    end if;

                    -- set the opcode
                    setByte(0, Unsigned_8(Operators'Pos(inst.operator)), code);
                    setByte(1, Unsigned_8(Register'Pos(inst.op1.regName)), code);
                    setByte(2, Unsigned_8(inst.op2.imm.immInt), code);
                    --setHiWord(Unsigned_32(inst.op2.imm.immInt), code);
                    Ada.Text_IO.Put_Line("CODEGEN: emitting G-Type code " & toHexString(code));
                    return True;

                -- Jump to label, expect a single large operand
                when HTypeOps =>
                    if inst.kind /= OneOperand and then inst.op1.kind /= ImmediateOperand then
                        msg := To_Unbounded_String("Error, line" & inst.line'Image & ": expected one immediate as operand for " & inst.operator'Image);
                        return False;
                    end if;

                    -- now we need to see if this immediate is an identifier
                    if not getImmediateIntegerValue(inst.op1.imm, immedInt) then
                        msg := To_Unbounded_String("Error, line" & inst.line'Image & ": immediate " & To_String(inst.op1.imm.immID) & " is either uninitialized or a floating point value");
                        return False;
                    end if;

                    -- set the opcode. Kinda lame, but we only support 32-bit absolute addresses
                    -- right now.
                        setHiWord(Unsigned_32(immedInt), code);
                        setByte(0, Unsigned_8(Operators'Pos(inst.operator)), code);
                        Ada.Text_IO.Put_Line("CODEGEN: emitting H-Type code " & toHexString(code));
                        return True;

                -- Jump to register (needs to be a GPR)
                when ITypeOps =>
                    if inst.kind /= OneOperand or inst.op1.kind /= RegisterOperand then
                        msg := To_Unbounded_String("Error, line" & inst.line'Image & ": expected one register as operand for " & inst.operator'Image);
                        return False;
                    end if;

                    if inst.op1.regName not in GeneralRegister then
                        msg := To_Unbounded_String("Error, line" & inst.line'Image & ": jumps only allowed to address in a general-purpose register. " & inst.operator'Image);
                        return False;
                    end if;
                    
                    setByte(0, Unsigned_8(Operators'Pos(inst.operator)), code);
                    setByte(1, Unsigned_8(Register'Pos(inst.op1.regName)), code);
                    Ada.Text_IO.Put_Line("CODEGEN: emitting I-Type code " & toHexString(code));
                    return True;

                -- Zero operands, like ret or relax
                when JTypeOps =>
                    if inst.kind /= NoOperand then
                        msg := To_Unbounded_String("Error, line" & inst.line'Image & ": expected zero operands for " & inst.operator'Image);
                        return False;
                    end if;             
                    
                    setByte(0, Unsigned_8(Operators'Pos(inst.operator)), code);
                    Ada.Text_IO.Put_Line("CODEGEN: emitting J-Type code " & toHexString(code));
                    return True;

                when others =>
                    msg := To_Unbounded_String("Error, line" & inst.line'Image & ": unsupported instruction type " & inst.operator'Image);
                    return False;
            end case;
        end genCode;

    begin
        -- just in case we're passed something with old values in it.
        objectFile.Clear;

        for inst of instructions loop
            if genCode(inst, opcode, msg) then
                objectFile.Append(New_Item => opcode);
            else
                return False;
            end if;
        end loop;

        return True;
    end codeGen;

end assembler;
