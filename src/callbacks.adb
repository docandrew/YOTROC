with Gdk.Event;
with Gdk.Types.Keysyms;

with Gtkada.Dialogs;
with Gtkada.File_Selection;

with Gtk.Dialog;
with Gtk.File_Chooser;
with Gtk.File_Chooser_Dialog; use Gtk.File_Chooser_Dialog;
with Gtk.List_Store;
with Gtk.Message_Dialog;
with Gtk.Main;
with Gtk.Status_Bar;
with Gtk.Text_Iter;
with Gtk.Tree_Model;
with Gtk.Widget;              use Gtk.Widget;

with Pango.Font;

with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;

with Interfaces;              use Interfaces;

with assembler;
with gui;
with util;
with vm;                      use vm;

-- Callback procedures, called from the Gtk GUI.
package body callbacks is
    -------------------------
    -- Some GUI state vars --
    -------------------------

    -- this is so we can pop up an "are you sure?" dialog on quit/new/open
    unsavedChanges : Boolean := False;

    -- current filename we are saving under.
    curFileName : Unbounded_String;

    -- Output from our assemble procedure. We'll pass this to the VM at boot time.
    machinecode : MachineCodeVector.Vector;
  
    -- gives us a chance to catch the signal to delete the main window. If we want
    -- to quit, return False.
    function tryQuit (Object : access Gtkada_Builder_Record'Class) return Boolean is
        pragma Unreferenced (Object);
    begin
        if unsavedChanges then 
            return not areYouSure;
        end if;

        return False;
    end tryQuit;

    procedure quit (Object : access Gtkada_Builder_Record'Class) is
    begin
        Gtk.Main.Main_Quit;
    end quit;

    ----------------------------------------------------------------------------
    -- assembleCB - callback for GUI "assemble" button.
    -- Assemble the source code into an object file that can be loaded into the
    -- VM's memory.
    ----------------------------------------------------------------------------
    procedure assembleCB (Object : access Gtkada_Builder_Record'Class) is
        pragma Unreferenced (Object);
        start : Gtk.Text_Iter.Gtk_Text_Iter;
        finish : Gtk.Text_Iter.Gtk_Text_Iter;
        assembly : Unbounded_String;
        result : Boolean;
        errMsg : Unbounded_String;
        instructions : assembler.InstructionVector.Vector;
        ignore : Gtkada.Dialogs.Message_Dialog_Buttons;
        ignore2 : Gtk.Status_Bar.Message_Id;
    begin
        Ada.Text_IO.Put_Line("Assemble");
        ignore2 := Gtk.Status_Bar.Push(gui.statusBar, 1, "Assembling...");

        gui.textbuf.Get_Bounds(start, finish);
        assembly := To_Unbounded_String(gui.textbuf.Get_Text(
                                         Start                => start,
                                         The_End              => finish,
                                         Include_Hidden_Chars => True));

        -- Assemble the file into a vector of instruction records
        result := assembler.parse(source       => To_String(assembly),
                                  instructions => instructions,
                                  msg          => errMsg);

        if not result then
            ignore := Gtkada.Dialogs.Message_Dialog(
                                  Msg            => To_String(errMsg),
                                  Dialog_Type    => Gtkada.Dialogs.Information,
                                  Buttons        => Gtkada.Dialogs.Button_OK,
                                  Title          => "Parse Error",
                                  Parent         => null);
            --gui.statusBar.removeAll;
            return;
        end if;

        --gui.statusBar.Push("Code gen...", Context => 0);
        -- convert the instruction vector into a vector of machine code insts.
        result := assembler.codeGen(instructions => instructions,
                                    objectFile   => machinecode,
                                    msg          => errMsg);
        if not result then
            ignore := Gtkada.Dialogs.Message_Dialog(
                                  Msg            => To_String(errMsg),
                                  Dialog_Type    => Gtkada.Dialogs.Information,
                                  Buttons        => Gtkada.Dialogs.Button_OK,
                                  Title          => "Code Generation Error",
                                  Parent         => null);
            --gui.statusBar.Remove_All;
            return;
        end if;

        --gui.statusBar.Push("Loading object file...", Context => 0);
        -- Load the opcodes into the machinecode list store.
        loadObjectFile : declare
            use Gtk.List_Store;
            use Gtk.Tree_Model;
            machinecodeListIter : Gtk_Tree_Iter;
            addr : Unsigned_64 := 0;
            inst : Unsigned_64 := 0;
        begin
            --machinecodeList := Gtk_List_Store(Gtkada.Builder.Get_Object(gui.builder, "machinecodeList"));
            gui.machinecodeList.Clear;

            Ada.Text_IO.Put_Line(" adding " & Integer(machinecode.Length)'Image & " instructions to liststore");

            for i in 0 .. Integer(machinecode.Length)-1 loop
                Ada.Text_IO.Put_Line(" adding element " & i'Image);
                inst := machinecode.Element(Integer(i));

                addr := Unsigned_64(Integer(i));

                gui.machinecodeList.Append(Iter => machinecodeListIter);
                gui.machinecodeList.Set(Iter => machinecodeListIter,
                                    Column => 0,
                                    Value => util.toHexString(addr));
                gui.machinecodeList.Set(Iter => machinecodeListIter,
                                    Column => 1,
                                    Value => util.toHexString(inst));
            end loop;
        end loadObjectFile;

        ignore2 := Gtk.Status_Bar.Push(gui.statusBar, 1, "Assembly Success!");
        -- Go ahead and boot the VM

        ignore := Gtkada.Dialogs.Message_Dialog(
                                  Msg            => "Assembly Finished, click OK to boot the VM.",
                                  Dialog_Type    => Gtkada.Dialogs.Information,
                                  Buttons        => Gtkada.Dialogs.Button_OK,
                                  Title          => "Success",
                                  Parent         => null);

        vm.boot(machinecode);
        gui.updateGUI_VM;
    end assembleCB;

    ----------------------------------------------------------------------------
    -- stepCB
    -- callback for the "step" button - instruct the VM to single-step an
    -- instruction.
    ----------------------------------------------------------------------------
    procedure stepCB (Object : access Gtkada_Builder_Record'Class) is
        pragma Unreferenced (Object);
        errMsg : Unbounded_String;
        healthy : Boolean;
        ignore : Gtkada.Dialogs.Message_Dialog_Buttons;
    begin
        Ada.Text_IO.Put_Line("Step");
        healthy := vm.step(errMsg);

        if not healthy then
            ignore := Gtkada.Dialogs.Message_Dialog(
                                  Msg            => To_String(errMsg),
                                  Dialog_Type    => Gtkada.Dialogs.Information,
                                  Buttons        => Gtkada.Dialogs.Button_OK,
                                  Title          => "Exception",
                                  Parent         => null);
            return;
        else
            gui.updateGUI_VM;
        end if;
    end stepCB;

    -----------
    -- runCB --
    -----------
    procedure runCB (Object : access Gtkada_Builder_Record'Class) is
        pragma Unreferenced (Object);
    begin
        Ada.Text_IO.Put_Line("Run");
    end runCB;

    ------------
    -- stopCB --
    ------------
    procedure stopCB (Object : access Gtkada_Builder_Record'Class) is
        pragma Unreferenced (Object);
    begin
        Ada.Text_IO.Put_Line("Stop");
    end stopCB;

    -----------
    -- newCB --
    -----------
    procedure newCB (Object : access Gtkada_Builder_Record'Class) is
        start : Gtk.Text_Iter.Gtk_Text_Iter;
        finish : Gtk.Text_Iter.Gtk_Text_Iter;
    begin
        if unsavedChanges then
            if not areYouSure then
                return;
            end if;
        end if;
        
        gui.textbuf.Get_Bounds(start, finish);
        gui.textbuf.Delete(Start => start, The_End => finish);
        unsavedChanges := False;
        curFileName := To_Unbounded_String("");
        gui.setTitle("YOTROC Assembler / Emulator: " & To_String(curFileName));
    end newCB;

    ------------
    -- openCB --
    ------------
    procedure openCB (Object : access Gtkada_Builder_Record'Class) is
        pragma Unreferenced(Object);

        openPath : Unbounded_String;
    begin
        if unsavedChanges then
            if not areYouSure then 
                return; 
            end if;
        end if;

        openPath := To_Unbounded_String(Gtkada.File_Selection.File_Selection_Dialog
          (Title => "Open",
           Must_Exist => True));

        if openPath /= "" then
            declare
                file : Ada.Text_IO.File_Type;
                contents : Unbounded_String;
                ignore : Gtkada.Dialogs.Message_Dialog_Buttons;
            begin
                Ada.Text_IO.Open(File => file,
                                 Mode => Ada.Text_IO.In_File,
                                 Name => To_String(openPath));

                -- read in entire file before assigning to text buffer.
                while not Ada.Text_IO.End_Of_File(file) loop
                    declare
                        use ASCII;
                        nextChr : Character;
--                        lookAhead : Character;
                    begin
                        Ada.Text_IO.Get_Immediate(file, nextChr);
                        Append(contents, nextChr);
                    end;
                end loop;

                --Ada.Text_IO.Put_Line("Open: " & To_String(openPath) & " contents: " & To_String(contents));
                Ada.Text_IO.Close(file);

                gui.textbuf.Set_Text(Text => To_String(contents));
                unsavedChanges := False;
                curFileName := openPath;
                gui.setTitle("YOTROC Assembler / Emulator: " & To_String(curFileName));

            exception
                when others =>
                ignore := Gtkada.Dialogs.Message_Dialog
                    (Msg => "Error opening file.",
                     Buttons => Gtkada.Dialogs.Button_OK,
                     Dialog_Type => Gtkada.Dialogs.Warning,
                     Title => "Uh oh!");
            end;
        end if;
    end openCB;

    -- underlying function for file saves
    procedure saveFile(savePath : Unbounded_String) is
        ignore : Gtkada.Dialogs.Message_Dialog_Buttons;
        file : Ada.Text_IO.File_Type;
        bufContents : Unbounded_String;
        start, finish : Gtk.Text_Iter.Gtk_Text_Iter;
    begin
        -- try to open the file
        openFile : declare
        begin
            -- open filepath, write it all out.
            Ada.Text_IO.Open(File => file, 
                             Mode => Ada.Text_IO.Out_File,
                             Name => To_String(savePath));
        exception
            when Ada.Text_IO.Name_Error =>
                Ada.Text_IO.Create(File => file,
                                   Mode => Ada.Text_IO.Out_File,
                                   Name => To_String(savePath));
        end openFile;

        gui.textbuf.Get_Bounds(start, finish);
        bufContents := To_Unbounded_String(gui.textbuf.Get_Text(Start => start, The_End => finish));

        Ada.Text_IO.Put_Line("Writing: ");
        for i in 1 .. Length(bufContents) loop
            Ada.Text_IO.Put(Element(bufContents, i));
            Ada.Text_IO.Put(file, Element(bufContents, i));
        end loop;

        -- if successful, make this the new curFileName
        curFileName := savePath;
        unsavedChanges := False;
        gui.setTitle("YOTROC Assembler / Emulator: " & To_String(curFileName));

        Ada.Text_IO.Close(file);
    exception
        when others =>
        ignore := Gtkada.Dialogs.Message_Dialog
           (Msg => "Error saving file.",
            Buttons => Gtkada.Dialogs.Button_OK,
            Dialog_Type => Gtkada.Dialogs.Warning,
            Title => "Uh oh!");
    end saveFile;

   ------------
   -- saveCB --
   ------------
    procedure saveCB (Object : access Gtkada_Builder_Record'Class) is
        savePath : Unbounded_String;
    begin
        if curFileName = "" then
            saveAsCB(Object);
        else
            saveFile(curFileName);
        end if;
    end saveCB;

   --------------
   -- saveAsCB --
   --------------
    procedure saveAsCB (Object : access Gtkada_Builder_Record'Class) is
        pragma Unreferenced(Object);
        savePath : Unbounded_String;
    begin
        savePath := To_Unbounded_String(Gtkada.File_Selection.File_Selection_Dialog
                                                   (Title       => "Save As",
                                                    Default_Dir => "",
                                                    Dir_Only    => False,
                                                    Must_Exist  => False));
        if savePath /= "" then
            saveFile(savePath);
        end if;

        return;
    end saveAsCB;

   -------------
   -- aboutCB --
   -------------
    procedure aboutCB (Object : access Gtkada_Builder_Record'Class) is
        use Gtk.Dialog;

        retButton : Gtkada.Dialogs.Message_Dialog_Buttons;
    begin
        retButton := Gtkada.Dialogs.Message_Dialog(
           Msg => "YOTROC Assembler & Emulator, written by Jon Andrew for Syracuse CIS655. All rights reserved.",
           Buttons => Gtkada.Dialogs.Button_OK,
           Dialog_Type => Gtkada.Dialogs.Information,
           Title => "About");

--        confirmDialog.Destroy;
    end aboutCB;

    ----------------------------------------------------------------------------
    --   editCB - called whenever changes are made to the textbuffer  
    ----------------------------------------------------------------------------
    procedure editCB (Object : access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class) is
    begin
        --TODO: it would be kind of fun to add syntax highlighting here.
        --Ada.Text_IO.Put_Line("textview changed");
        unsavedChanges := True;
    end editCB;

    ---------------------------------------------------------------------------
    -- confirmUnsaved
    -- pop up a dialog confirming that they want to exit/open file, etc.
    -- when there are unsaved changes. Return True if they want to proceed.
    ---------------------------------------------------------------------------

    function areYouSure return Boolean is
        use Gtk.Dialog;

        confirmDialog : Gtk.Message_Dialog.Gtk_Message_Dialog;
        confirmYesNo : Gtk.Dialog.Gtk_Response_Type;
        confirmFlags : Gtk.Dialog.Gtk_Dialog_Flags;
    begin
        confirmFlags := Gtk.Dialog.Modal;
        confirmDialog := Gtk.Message_Dialog.Gtk_Message_Dialog_New
          (Parent => null,
           The_Type => Gtk.Message_Dialog.Message_Question,
           Flags => confirmFlags,
           Buttons => Gtk.Message_Dialog.Buttons_Yes_No,
           Message => "You have unsaved changes, are you sure you want to do this?");

        confirmYesNo := confirmDialog.Run;

        confirmDialog.Destroy;

        return confirmYesNo = Gtk.Dialog.Gtk_Response_Yes;
    end areYouSure;

end callbacks;
