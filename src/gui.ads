with Gtkada.Builder;
with Gtk.List_Store;
with Gtk.Status_Bar;
with Gtk.Text_Buffer;
with Gtk.Widget;

package gui is

    builder : Gtkada.Builder.Gtkada_Builder;
    topLevelWindow : Gtk.Widget.Gtk_Widget;
    textbuf : Gtk.Text_Buffer.Gtk_Text_Buffer;
    statusBar : Gtk.Status_Bar.Gtk_Status_Bar;
    --statusBarContext : Gtk.Status_Bar.Context_Id;

    machinecodeList : Gtk.List_Store.Gtk_List_Store;
    memoryList : Gtk.List_Store.Gtk_List_Store;
    registerList : Gtk.List_Store.Gtk_List_Store;

    procedure load;
    procedure setTitle(newTitle : String);
    procedure updateGUI_VM;
end gui;
