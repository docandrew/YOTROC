--with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Text_IO;
--with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

--with Interfaces;               use Interfaces;



--with assembler;                use assembler;
--with callbacks;                use callbacks;
with gui;
--with util;                     use util;
--with vm;                       use vm;

procedure Main is

begin
    gui.load;
    Ada.Text_IO.Put_Line("exited.");
end Main;
