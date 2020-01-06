package body tests is

    testVal : Unsigned_64 := 0;

    procedure runTests is
        use ASCII;
        parseResult : Boolean;
        codeGenResult : Boolean;
        errmsg : Unbounded_String;
        insts : InstructionVector.Vector;

        -- a bit hacky... short-hand for To_Unbounded_String
        function "-" (Source : String) return Unbounded_String
            renames Ada.Strings.Unbounded.To_Unbounded_String;

        type TestPrograms is array (positive range <>) of Unbounded_String;
        parserTests1 : TestPrograms := (
    -- test comments
        -";this is a comment" & LF &
        ";another comment",         --1 OK, just comments

    -- test directives
        -";this is a comment" & LF &
        "% abcd 5678" & LF &        --2 no directive after %
        ";comment 2",

        -";this is a comment" & LF &
        "%declare abcd 5678" & LF & --3 "declare" not understood
        ";comment 2",

    -- test identifiers
        -"%define 6asdf 0d1234",    --4 identifier must start with letter

        -"%define as!df 0d1234",    --5 ident. only letters, nums and _

        -"%define asdf! 0d1234",    --6 ident. only letters, nums and _

        -"%define a_sdf 1234",      --7 OK

    -- test numeric literals
        -"%define asdf 1234",       --8 OK.

        -"%define asdf 2#0101#",    --9 OK, use Ada format for literals

        -"%define asdf 16#FFFF#",   --10 OK

        -"%define asdf 123.45",     --11 floats OK

        -"%define asdf 123.4.5",    --12 err, extra decimal.

        -"%define asdf 347" & LF &  --13 OK, for testing identifier map.
         "%define foo 43" & LF &
         "%define bar 99",

  -- test labels
        -"; Comment" & LF &
         "@label ;at address 0",    --14 OK, address at 0

        -"@2badlabel",              --15 Bad, label must be a valid identifier

  -- test instructions
        -"sysret",                  --16 OK, no operands

        -"cb R1 R3",                --17 OK, count bits, two operands

        -"l8 R1 R2",                --18 OK, two operands

        -"add R1 R2 R3",            --19 OK, three operands

        -"l32 R1 *R2",              --20 OK, reg indirect

        -"s32 R1 *(R2 + 1)",        --21 OK, reg displacement

        -"s32 R1 *(r2 + 16#00FF#)", --22 OK, reg displacement with hex

        -"mov R1 R2",               --23 err, unrecognized instruction

        -"s32 R1 booger",           --24 OK, identifier

        -"@mylabel" & LF &
         "jmpa mylabel",            --25 OK, jumping to label

        -"jmpa mylabel" & LF &
         "@mylabel",                --26 OK, jumping to forward label

        -"s32 R1 R2;a comment",     --27 OK, comment right after

        -"s32 R1 R2   ;a comment",  --28 OK, whitespace and comment after operand

        -"sysret; a comment",       --29 OK, comment right after instruction no operand

        -"s32 R1 *(R2 + 44",        --30 err, no closing paren.

        -"l32 R1 16#4040#",         --31 OK, immediate operand

        -"sysret    ;a comment");   --32 OK, whitespace and comment after no operand


    begin
        for pg in parserTests1'Range loop
            insts.Clear;
            machinecode.Clear;

            Ada.Text_IO.Put_Line("" & LF & LF & "Parsing test program " & pg'Image);
            parseResult := assembler.parse(To_String(parserTests1(pg)), insts, errmsg);

            if parseResult then
                Ada.Text_IO.Put_Line("Parse OK" & pg'Image);

                Ada.Text_IO.Put_Line("Identifiers Found: ");
                dumpIdentifiers;

            -- test code gen if parsing was successful
                codeGenResult := assembler.codeGen(insts, machinecode, errmsg);
                if codeGenResult then
                    Ada.Text_IO.Put_Line("OK codegen" & pg'Image);
                else
                    Ada.Text_IO.Put_Line("");
                    Ada.Text_IO.Put("CODEGEN ERR" & pg'Image & " ");
                    Ada.Text_IO.Unbounded_IO.Put_Line(errmsg);
                end if;
            else
                Ada.Text_IO.Put_Line("");
                Ada.Text_IO.Put("PARSE ERR" & pg'Image & " ");
                Ada.Text_IO.Unbounded_IO.Put_Line(errmsg);
            end if;

        end loop;
    end runTests;

end tests;
