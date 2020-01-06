with Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

with Interfaces;               use Interfaces;

with Glib;
with Glib.Error;               use Glib.Error;
with Glib.Object;

with Gdk.Cursor;

with Gtk;
with Gtk.Application_Window;
with Gtk.Box;
with Gtk.Editable;
with Gtk.GEntry;
with Gtk.Handlers;
with Gtk.List_Store;
with Gtk.Tree_Model;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Main;
with Gtk.Status_Bar;
with Gtk.Text_View;
with Gtk.Text_Buffer;
with Gtk.Window;

with Gtkada.Builder;           use Gtkada.Builder;

with Pango.Font;

with callbacks;
with util;
with vm;

package body gui is

    --use ASCII;
    error : aliased Glib.Error.GError;
    returnCode : Glib.Guint;
    contextDescription : String := "normal";

    procedure registerHandlers is
    begin

        builder.Register_Handler(Handler_Name => "try_quit_cb", Handler => callbacks.tryQuit'Access);
        builder.Register_Handler(Handler_Name => "Main_Quit", Handler => callbacks.quit'Access);

        builder.Register_Handler(Handler_Name => "assembleButton1_clicked_cb", Handler => callbacks.assembleCB'Access);
        builder.Register_Handler(Handler_Name => "stepButton1_clicked_cb", Handler => callbacks.stepCB'Access);
        --builder.Register_Handler(Handler_Name => "runButton1_clicked_cb", Handler => callbacks.runCB'Access);
        --builder.Register_Handler(Handler_Name => "stopButton_clicked_cb", Handler => callbacks.stopCB'Access);

        builder.Register_Handler(Handler_Name => "newMenuItem_activate_cb", Handler => callbacks.newCB'Access);
        builder.Register_Handler(Handler_Name => "openMenuItem_activate_cb", Handler => callbacks.openCB'Access);
        builder.Register_Handler(Handler_Name => "saveMenuItem_activate_cb", Handler => callbacks.saveCB'Access);
        builder.Register_Handler(Handler_Name => "saveAsMenuItem_activate_cb", Handler => callbacks.saveAsCB'Access);

        builder.Register_Handler(Handler_Name => "aboutMenu_activate_cb", Handler => callbacks.aboutCB'Access);

        -- the normal register handler function doesn't work here
        textbuf := Gtk.Text_View.Gtk_Text_View(Gtkada.Builder.Get_Object(builder, "textview1")).Get_Buffer;
        callbacks.text.Connect(Widget => textbuf, Name => Gtk.Text_Buffer.Signal_Changed, Cb => callbacks.editCB'Access);
        --builder.Register_Handler(Handler_Name => "textview1_key_release_event_cb", Handler => callbacks.keypressCB'Access);
    end registerHandlers;

    -- Load our GUI description from the XML file and display it.
    procedure load is
        use Gtk.Box;
        use Gtk.List_Store;
        use Gtk.Status_Bar;
        -- font for the GtkTextView
        font : Pango.Font.Pango_Font_Description;
        textbufObj : Glib.Object.GObject;
        textbufWidget : Gtk_Widget;
        vbox : Gtk.Box.Gtk_Vbox;
    begin

        Gtk.Main.Init;

        Gtkada.Builder.Gtk_New (builder);

        returnCode := Gtkada.Builder.Add_From_File(builder, "../yotroc_gui.xml", error'Access);
        if error /= null then
            Ada.Text_IO.Put_Line("Error: " & Get_Message(error));
            Error_Free(error);
            return;
        end if;

        registerHandlers;

        Gtkada.Builder.Do_Connect (builder);

        -- pull references to the various GTK widgets we defined in the GUI .xml file.
        topLevelWindow := Gtk_Widget(Gtkada.Builder.Get_Object(builder, "applicationwindow1"));

        machinecodeList := Gtk_List_Store(Gtkada.Builder.Get_Object(gui.builder, "machinecodeList"));
        memoryList := Gtk_List_Store(Gtkada.Builder.Get_Object(gui.builder, "memoryList"));
        registerList := Gtk_List_Store(Gtkada.Builder.Get_Object(gui.builder, "registerList"));
        vbox := Gtk_VBox(Gtkada.Builder.Get_Object(gui.builder, "vbox"));

        -- we add the status bar manually because for whatever reason Glade didn't like our status bar
        Gtk.Status_Bar.Gtk_New(statusBar);
        Pack_End(vbox, statusBar, False, False, 0);
        
        --statusBar := Gtk_Status_Bar(Gtkada.Builder.Get_Object(gui.builder, "statusBar1"));

        if statusBar = null then
            Ada.Text_IO.Put_Line("status bar null");
        end if;

        font := Pango.Font.To_Font_Description(Family_Name => "Monospace", Size => Glib.Gint(11));
        textbufObj := Gtkada.Builder.Get_Object(builder, "textview1");
        textbuf := Gtk.Text_View.Gtk_Text_View(textbufObj).Get_Buffer;
        textbufWidget := Gtk_Widget(textbufObj);
        textbufWidget.Modify_Font(font);

        Gtk.Widget.Show_All(topLevelWindow);

    --  Start the Gtk+ main loop (blocked until Gtk.Main.Quit called in callbacks)
        Gtk.Main.Main;
        Unref(Builder);
    end;

    -- set the application window title
    procedure setTitle(newTitle : String) is
        use Gtk.Application_Window;

        appWindow : Gtk_Application_Window;
    begin
        appWindow := Gtk_Application_Window(Gtkada.Builder.Get_Object(builder, "applicationwindow1"));
        appWindow.Set_Title(Title => newTitle);
    end setTitle;

    -----------------------------------------------------------------------------
    -- updateGUI_VM
    -- poll the VM and update the register and memory contents on the GUI with
    -- what the VM is showing.
    -----------------------------------------------------------------------------
    procedure updateGUI_VM is
        use Gtk.List_Store;
        use Gtk.Tree_Model;
        use vm;
        listIter : Gtk_Tree_Iter;
        --memListIter : Gtk_Tree_Iter;
        status : Unbounded_String;
        ret : Gtk.Status_Bar.Message_Id;
    begin
        -- for now, just blow away and reload the list each time. We'll figure out
        -- how to do updates later.
        registerList.Clear;
        listIter := registerList.Get_Iter_First;
        --Ada.Text_IO.Put_Line(" adding " & Integer(machinecode.Length)'Image & " instructions to liststore");

        for i in vm.regs'Range loop
            --Ada.Text_IO.Put_Line(" adding element to registerList " & i'Image);
            registerList.Append(Iter => listIter);
            registerList.Set(Iter => listIter,
                                Column => 0,
                                Value => Register'Image(i));

            -- display floating-point values natively
            if i in vm.FloatRegister then
                registerList.Set(Iter => listIter,
                                 Column => 1,
                                 Value => util.toDouble(vm.regs(i))'Image);
            else
                registerList.Set(Iter => listIter,
                                Column => 1,
                                Value => util.toHexString(vm.regs(i)));
            end if;
        end loop;

        memoryList.Clear;
        listIter := memoryList.Get_Iter_First;
        for i in vm.memory'Range loop
            --Ada.Text_IO.Put_Line(" adding element to memoryList " & i'Image);
            memoryList.Append(Iter => listIter);
            memoryList.Set(Iter => listIter,
                           Column => 0,
                           Value => util.toHexString(Unsigned_64(Natural(i))));
            memoryList.Set(Iter => listIter,
                           Column => 1,
                           Value => util.toHexString(vm.memory(Natural(i))));
        end loop;

        status := To_Unbounded_String("PC: " & util.toHexString(vm.regs(pc)) & " Z: " & vm.flags.zero'Image &
                                       " OF: " & vm.flags.overflow'Image & " EQ: " & vm.flags.eq'Image);

        --statusBarContext := Get_Context_Id(Context_Description => contextDescription);
        --statusBar.Remove_All(Context => statusBarContext);
        ret := Gtk.Status_Bar.Push(statusBar, 1, To_String(status));
    end updateGUI_VM;

end gui;
