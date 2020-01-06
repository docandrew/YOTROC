-- Recursive Descent parser for YOTROC instruction set
-- 
-- Registers: 64 general purpose registers
--            64 floating-point registers
--            64-bit FLAGS register
--            64-bit program counter
--
-- label: keep a separate "label table" with addresses of the instruction
-- immediately after. Anything that ends with a ':' is a label.
--
-- db, ds, dq : declare bytes, shorts, quads in memory
--
-- define variable value -- simple text substitution, keep separate table
--
-- Addressing Modes:
-- Immediate: DECNUM, 16#HEXNUM#, 2#BINNUM#
-- Register: R1, R2
-- Indirect: *R1, *R2, meaning use memory address in that register
-- Memory: *0dDECNUM, *0xHEXNUM, use memory address
-- Indexed: *(R1 + immediate)

with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces; use Interfaces;

with vm; use vm;

package assembler is
    
    ---------------------------------------------------------------------------
    -- The types below look strange. Ada calls them "Variant Records", but they
    -- work much like Sum types in other languages.
    ---------------------------------------------------------------------------

    -- identifiers are just strings, but need to obey rules:
    -- has to start with leading alpha char, and alphanumeric only
    subtype Identifier is Unbounded_String;

    type RealKind is (IntegerReal, FloatReal);

    -- When storing identifiers, they need to map to some sort of 
    -- numeric type. We represent that here. Ada already has a Real
    -- type, so make up a new one. This is only a "Real" number in
    -- the sense of our lexical analysis. A YOTROCReal can store either
    -- an Integer or a Floating-Point number.
    type YOTROCReal (kind : RealKind := FloatReal) is
        record
            case kind is
                when IntegerReal =>
                    realInt : Integer;
                when FloatReal =>
                    realFloat : Float;
            end case;
        end record;


    type ImmediateKind is (IdentifierImm, IntegerImm, FloatImm);
       
    -- An immediate can either be a variable name, an integer,
    -- or a floating-point value.
    type Immediate (kind : ImmediateKind := FloatImm) is
        record
            case kind is
                when IdentifierImm =>
                    -- will need to attempt a lookup of this identifier in
                    -- the identifier table and see if there's a value for it.
                    immID : Unbounded_String;
                when IntegerImm =>
                    immInt : Integer;
                when FloatImm =>
                    immFloat : Float;
            end case;
        end record;

    type OperandKind is (ImmediateOperand, RegisterOperand, Displacement);

    -- Variant Record, specify default kind so we can keep these as part of an array
    type Operand (kind : OperandKind := Displacement) is
        record
            case kind is
                when ImmediateOperand =>
                    imm : Immediate;
                when RegisterOperand =>
                    regName : Register;
                -- double up here for register indirect as well, since we can
                -- just set offset = 0.
                when Displacement =>
                    regBase : GeneralRegister;
                    offset : Integer;
            end case;
       end record;

    --type RegisterKind is (GeneralReg, FloatingReg);
    type InstructionKind is (NoOperand, OneOperand, TwoOperand, ThreeOperand);

    -- Variant record with up to 3 operands for this instruction
    type Instruction (kind : InstructionKind := ThreeOperand) is
        record
            line : Integer;            -- line number this instruction was at
            operator : Operators;

            case kind is
                when NoOperand =>
                   null;
                when OneOperand | TwoOperand | ThreeOperand =>
                    op1 : Operand;
                    case kind is
                        when TwoOperand | ThreeOperand =>
                            op2 : Operand;
                            case kind is
                                when ThreeOperand =>
                                    op3 : Operand;
                                when others =>
                                    null;
                            end case;
                        when others =>
                            null;
                    end case;
            end case;
        end record;

    
    -- output from parser
    package InstructionVector is new Vectors(Natural, Instruction);
    use InstructionVector;

    procedure dumpIdentifiers;
    
    function parse(source : in String; 
                   instructions : out InstructionVector.Vector; 
                   msg : out Unbounded_String) return Boolean;

    function codeGen(instructions : in InstructionVector.Vector;
                     objectFile : out MachineCodeVector.Vector;
                     msg : out Unbounded_String) return Boolean;
        
end assembler;
