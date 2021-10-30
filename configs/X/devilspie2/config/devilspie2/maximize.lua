debug_print("Window Name: " .. get_window_name());
debug_print("Application name: " .. get_application_name())
debug_print("Window class: " .. get_window_class());
-- debug_print("Window class instance: " .. get_class_instance_name());
debug_print("Window type: " .. get_window_type());
debug_print("Window role: " .. get_window_role());

-- Make Iceweasel always start maximized.
if ((get_window_class()=="Gnome-terminal" or
     get_window_class()=="Emacs" or
     get_window_class()=="Xfce4-terminal" or
     get_window_class()=="Hexchat" or
     get_window_class()=="Evince" or
     get_window_class()=="Geeqie" or
     get_window_class()=="Google-chrome")
    and (not get_window_is_maximized() and get_window_type()=="WINDOW_TYPE_NORMAL" )) then
   maximize()
   focus()
end