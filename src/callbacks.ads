with Gtkada.Builder;   use Gtkada.Builder;
with Gtk.GEntry;
with Gtk.Widget;
with Gtk.Handlers;
with Gtk.Text_View;
with Gtk.Text_Buffer;
with Glib.Values;

package callbacks is

    package text is new Gtk.Handlers.Callback (Gtk.Text_Buffer.Gtk_Text_Buffer_Record);

    function tryQuit (Object : access Gtkada_Builder_Record'Class) return Boolean;
    procedure quit (Object : access Gtkada_Builder_Record'Class);
    procedure assembleCB (Object : access Gtkada_Builder_Record'Class);
    procedure stepCB (Object : access Gtkada_Builder_Record'Class);
    procedure runCB (Object : access Gtkada_Builder_Record'Class);
    procedure stopCB (Object : access Gtkada_Builder_Record'Class);

    procedure newCB (Object : access Gtkada_Builder_Record'Class);
    procedure openCB (Object : access Gtkada_Builder_Record'Class);
    procedure saveCB (Object : access Gtkada_Builder_Record'Class);
    procedure saveAsCB (Object : access Gtkada_Builder_Record'Class);

    procedure aboutCB (Object : access Gtkada_Builder_Record'Class);

    procedure editCB (Object : access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class);

    function areYouSure return Boolean;
end callbacks;
