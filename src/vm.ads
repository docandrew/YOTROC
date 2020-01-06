--
-- YOTROC virtual machine
--
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;      use Ada.Containers;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Interfaces;                      use Interfaces;

with util;                            use util;

package vm is

    MAX_ADDRESS : constant := 16#00FF#;

    -- output from codegen
    package MachineCodeVector is new Vectors(Natural, Unsigned_64);
    use MachineCodeVector;

    -- List of all the instruction operators in YOTROC
    -- Note that the opcode for each of these instructions is just it's position
    -- in the list.
    type Operators is (
-------------------------------------------------------------------------------
-- Zero operand instructions (J TYPE)
-------------------------------------------------------------------------------
        relax,      -- just chill. Called "nop" in less luxurious architectures.
        avast,      -- pull this Yacht back into port and shut down.
        ret,        -- return from function
-------------------------------------------------------------------------------
-- LOAD/STORE instructions (A,B,C,D TYPE for each of these)
-------------------------------------------------------------------------------
-- LOAD: TO: Register, FROM: register, register indirect, displacement, immediate
        l8,
        l16,
        l32,        -- load lower half (for immediates)
        l32u,       -- load upper half (for immediates)
        l64,

-- STORE: FROM: Register TO: register indirect, displacement
        s8,
        s16,
        s32,
        s64,

-------------------------------------------------------------------------------
-- Single-Bit instructions (G TYPE)
-------------------------------------------------------------------------------
-- BIT OPERATIONS (F-Type)
        btc,    --R, bit   -- R = R and (not 1 << bit) (bit clear)
        bts,    --R, bit   -- R = R or (1 << bit)      (bit set)
        tb,     --R, bit   -- test bit, sets Zero flag 

-------------------------------------------------------------------------------
-- Arithmetic (3 operand) instructions (E TYPE)
-------------------------------------------------------------------------------
-- ARITHMETIC
        add,    -- R1 R2 R3 : R1 = R2 + R3
        fadd,   -- f1 f2 f3 (FP)
        sub,    -- R1 R2 R3 : R1 = R2 - R3
        fsub,   -- f1 f2 f3 (FP)
        mul,    -- R1 R2 R3 : R1 = R2 * R3
        fmul,   -- f1 f2 f3 (FP)
        div,    -- R1 R2 R3 : R1 = R2 / R3
        fdiv,   -- f1 f2 f3 (FP)
-- Note: append 'b' to these because they're keywords in Ada. Lame, but works.
        modb,   -- R1 R2 R3 : R1 = R2 mod R3
        orb,    -- R1 R2 R3 : R1 = R2 or R3  (bitwise)
        andb,   -- R1 R2 R3 : R1 = R2 and R3 (bitwise)
        xorb,   -- R1 R2 R3 : R1 = R2 xor R3 (bitwise)
-- YACHT instructions
        int,    -- R1 R2 R3 : R1 = Principal, R2 = interest rate (float),
                --  R3 = time. Assumes annual compounding. Stores result back in
                -- R1.
-------------------------------------------------------------------------------
-- Arithmetic (2 operand) instructions (F TYPE)
-------------------------------------------------------------------------------
-- YACHT instructions
        knots,  -- R1 R2    : convert R1 miles per hour into R1 knots.
        miles,  -- R1 R2    : convert R2 knots to R1 miles per hour.
-- INTEGER to DOUBLE
        itd,    -- f1 R1   : F1 = FP representation of R1
-- SINGLE-TO-DOUBLE PRECISION FP
        std,    -- f1 f2   : f1 = double version of f2, if f2 is single-precision
-- COUNT BITS
        cb,     -- R1 R2   : R1 = # of bits set in R2
-- LOGICAL SHIFTS
        shll,   -- R1 R2   : R1 << R2
        shrl,   -- R1 R2   : R1 >> R2
-- ARITHMETIC SHIFTS
        --shla,   -- R1 R2   : R1 << R2 (same as logical left shift)
        --shra,   -- R1 R2   : R1 >> R2, fill vacancies with MSB
-- ROTATE
        --rotl,   -- R1 R2   : R1 rotl R2
        --rotr,   -- R1 R2   : R1 rotr R2
-- COMPARISON
        cmp,    -- R1 R2   : compare contents in registers
-------------------------------------------------------------------------------
-- One-Operand Instructions (I-Type)
-------------------------------------------------------------------------------
-- BITWISE NOT
        notb,   -- R1       : R1 = ~R1 (bitwise)
-- BRANCHES
        jmp,    -- R1       : jump unconditional
        jz,     -- R1       : jump to address in R if zero flag set
        jeq,    -- R1       : jump to address in R if if comparison was equal
        jne,    -- R1       : jump to address in R if not equal
        jlt,    -- R1       : jump to address in R if less than
        jgt,    -- R1       : jump to address in R if greater than
-- FUNCTION CALLS
        call,   -- R1       : call function (jump and link)

-------------------------------------------------------------------------------
-- One-Operand Immediate Instructions (H-Type)
-------------------------------------------------------------------------------
-- "Jump Absolute"
        jmpa,   -- immed. address
        jza,    -- immed. address
        jeqa,   -- immed. address
        jnea,   -- immed. address
        jlta,   -- immed. address
        jgta    -- immed. address
        );

    -- arrange these by number of operands
    subtype ABCDOps is Operators range l8 .. s64;
    subtype ETypeOps is Operators range add .. int;
    subtype FTypeOps is Operators range knots .. cmp;
    subtype GTypeOps is Operators range btc .. tb;
    subtype HTypeOps is Operators range jmpa .. jgta;
    subtype ITypeOps is Operators range notb .. call;
    subtype JTypeOps is Operators range relax .. ret;
        
    -- List of the registers in our system.
    type Register is (
                        r0,  r1,  r2,  r3,  r4,  r5,  r6,  r7,  r8,  r9,  r10, r11, r12, r13, 
                        r14, r15,
                        r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29,
                        r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43,
                        r44, r45, r46, r47, r48, r49, r50, r51, r52, r53, r54, r55, r56, r57,
                        r68, r59, r60, r61, r62, r63,
                        f0,  f1,  f2,  f3,  f4,  f5,  f6,  f7,  f9,  f10, f11, f12, f13, f14, f15,
                        f16, f17, f18, f19, f20, f21, f22, f23, f24, f25, f26, f27, f28, f29,
                        f30, f31, f32, f33, f34, f35, f36, f37, f38, f39, f40, f41, f42, f43,
                        f44, f45, f46, f47, f48, f49, f50, f51, f52, f53, f54, f55, f56, f57,
                        f68, f59, f60, f61, f62, f63,
                        z, pc);

    subtype GeneralRegister is Register range r0 .. r63;
    subtype FloatRegister is Register range f0 .. f63;
    subtype SpecialRegister is Register range z .. pc;

    --type GPRFile is array (RegisterID'Pos
    
    -- Registers and main memory for our Virtual Machine
    --pc : Unsigned_64 := 0;
    --z : Unsigned_64 := 0;
    --flags : Unsigned_64 := 0;
    regs : array (Register'First .. Register'Last) of Unsigned_64;
    --fpregs : array (FloatRegister'First .. FloatRegister'Last) of Double;
    --spregs : array (SpecialRegister'First .. SpecialRegister'Last) of Unsigned_64;
    memory : array (0..MAX_ADDRESS) of Unsigned_64;
    
    type FlagsType is
        record
            zero : Boolean;
            lt : Boolean;
            gt : Boolean;
            eq : Boolean;
            overflow : Boolean;
            sign : Boolean;
            carry : Boolean;
        end record;

    flags : FlagsType;

    -- top two bits of operand are modifiers, used in load/store instructions
    -- to indicate what type it is.
    loadStoreImmModifier : constant := 0;      -- 00
    loadStoreRegModifier : constant := 128;    -- 10
    loadStoreIndModifier : constant := 64;     -- 01
    loadStoreDisModifier : constant := 192;    -- 11

    function step(msg : out Unbounded_String) return Boolean;
    procedure boot(objectFile : in MachineCodeVector.Vector);

    --procedure dumpRegs;
    
end vm;
