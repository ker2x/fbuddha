C
C
C
C
C   generated by makejapif   DO NOT EDIT
C
C   C
C



C      BOOLEAN
        PARAMETER ( J_TRUE              =    1)
        PARAMETER ( J_FALSE             =    0)



C      ALIGNMENT
        PARAMETER ( J_LEFT              =    0)
        PARAMETER ( J_CENTER            =    1)
        PARAMETER ( J_RIGHT             =    2)
        PARAMETER ( J_TOP               =    3)
        PARAMETER ( J_BOTTOM            =    4)
        PARAMETER ( J_TOPLEFT           =    5)
        PARAMETER ( J_TOPRIGHT          =    6)
        PARAMETER ( J_BOTTOMLEFT        =    7)
        PARAMETER ( J_BOTTOMRIGHT       =    8)



C      CURSOR
        PARAMETER ( J_DEFAULT_CURSOR    =    0)
        PARAMETER ( J_CROSSHAIR_CURSOR  =    1)
        PARAMETER ( J_TEXT_CURSOR       =    2)
        PARAMETER ( J_WAIT_CURSOR       =    3)
        PARAMETER ( J_SW_RESIZE_CURSOR  =    4)
        PARAMETER ( J_SE_RESIZE_CURSOR  =    5)
        PARAMETER ( J_NW_RESIZE_CURSOR  =    6)
        PARAMETER ( J_NE_RESIZE_CURSOR  =    7)
        PARAMETER ( J_N_RESIZE_CURSOR   =    8)
        PARAMETER ( J_S_RESIZE_CURSOR   =    9)
        PARAMETER ( J_W_RESIZE_CURSOR   =    10)
        PARAMETER ( J_E_RESIZE_CURSOR   =    11)
        PARAMETER ( J_HAND_CURSOR       =    12)
        PARAMETER ( J_MOVE_CURSOR       =    13)



C      ORIENTATION
        PARAMETER ( J_HORIZONTAL        =    0)
        PARAMETER ( J_VERTICAL          =    1)



C      FONTS
        PARAMETER ( J_PLAIN             =    0)
        PARAMETER ( J_BOLD              =    1)
        PARAMETER ( J_ITALIC            =    2)
        PARAMETER ( J_COURIER           =    1)
        PARAMETER ( J_HELVETIA          =    2)
        PARAMETER ( J_TIMES             =    3)
        PARAMETER ( J_DIALOGIN          =    4)
        PARAMETER ( J_DIALOGOUT         =    5)



C      COLORS
        PARAMETER ( J_BLACK             =    0)
        PARAMETER ( J_WHITE             =    1)
        PARAMETER ( J_RED               =    2)
        PARAMETER ( J_GREEN             =    3)
        PARAMETER ( J_BLUE              =    4)
        PARAMETER ( J_CYAN              =    5)
        PARAMETER ( J_MAGENTA           =    6)
        PARAMETER ( J_YELLOW            =    7)
        PARAMETER ( J_ORANGE            =    8)
        PARAMETER ( J_GREEN_YELLOW      =    9)
        PARAMETER ( J_GREEN_CYAN        =    10)
        PARAMETER ( J_BLUE_CYAN         =    11)
        PARAMETER ( J_BLUE_MAGENTA      =    12)
        PARAMETER ( J_RED_MAGENTA       =    13)
        PARAMETER ( J_DARK_GRAY         =    14)
        PARAMETER ( J_LIGHT_GRAY        =    15)
        PARAMETER ( J_GRAY              =    16)



C      BORDERSTYLE
        PARAMETER ( J_NONE              =    0)
        PARAMETER ( J_LINEDOWN          =    1)
        PARAMETER ( J_LINEUP            =    2)
        PARAMETER ( J_AREADOWN          =    3)
        PARAMETER ( J_AREAUP            =    4)



C      MOUSELISTENER
        PARAMETER ( J_MOVED             =    0)
        PARAMETER ( J_DRAGGED           =    1)
        PARAMETER ( J_PRESSED           =    2)
        PARAMETER ( J_RELEASED          =    3)
        PARAMETER ( J_ENTERERD          =    4)
        PARAMETER ( J_EXITED            =    5)
        PARAMETER ( J_DOUBLECLICK       =    6)



C      COMPONENTLISTENER



C      J_MOVED
        PARAMETER ( J_RESIZED           =    1)
        PARAMETER ( J_HIDDEN            =    2)
        PARAMETER ( J_SHOWN             =    3)



C      WINDOWLISTENER
        PARAMETER ( J_ACTIVATED         =    0)
        PARAMETER ( J_DEACTIVATED       =    1)
        PARAMETER ( J_OPENED            =    2)
        PARAMETER ( J_CLOSED            =    3)
        PARAMETER ( J_ICONIFIED         =    4)
        PARAMETER ( J_DEICONIFIED       =    5)
        PARAMETER ( J_CLOSING           =    6)



C      IMAGEFILEFORMAT
        PARAMETER ( J_GIF               =    0)
        PARAMETER ( J_JPG               =    1)
        PARAMETER ( J_PPM               =    2)
        PARAMETER ( J_BMP               =    3)



C      LEDFORMAT
        PARAMETER ( J_ROUND             =    0)
        PARAMETER ( J_RECT              =    1)



C      RANDOMMAX
        PARAMETER ( J_RANDMAX           =    2147483647)




        logical       j_start 
        logical       j_connect 
        integer       j_frame 
        integer       j_button 
        integer       j_graphicbutton 
        integer       j_checkbox 
        integer       j_label 
        integer       j_graphiclabel 
        integer       j_canvas 
        integer       j_panel 
        integer       j_borderpanel 
        integer       j_radiogroup 
        integer       j_radiobutton 
        integer       j_list 
        integer       j_choice 
        integer       j_dialog 
        integer       j_window 
        integer       j_popupmenu 
        integer       j_scrollpane 
        integer       j_hscrollbar 
        integer       j_vscrollbar 
        integer       j_line 
        integer       j_printer 
        integer       j_image 
        integer       j_messagebox 
        integer       j_alertbox 
        integer       j_choicebox2 
        integer       j_choicebox3 
        integer       j_progressbar 
        integer       j_led 
        integer       j_sevensegment 
        integer       j_meter 
        integer       j_textfield 
        integer       j_textarea 
        integer       j_menubar 
        integer       j_menu 
        integer       j_helpmenu 
        integer       j_menuitem 
        integer       j_checkmenuitem 
        integer       j_sound 
        logical       j_getstate 
        integer       j_getrows 
        integer       j_getcolumns 
        integer       j_getselect 
        logical       j_isselect 
        logical       j_isvisible 
        logical       j_isparent 
        logical       j_isresizable 
        integer       j_getlength 
        integer       j_getvalue 
        integer       j_getdanger 
        integer       j_getscreenheight 
        integer       j_getscreenwidth 
        integer       j_getheight 
        integer       j_getwidth 
        integer       j_getinsets 
        integer       j_getlayoutid 
        integer       j_getinheight 
        integer       j_getinwidth 
        integer       j_getitemcount 
        integer       j_getselstart 
        integer       j_getselend 
        integer       j_getcurpos 
        integer       j_getaction 
        integer       j_nextaction 
        integer       j_getviewportheight 
        integer       j_getviewportwidth 
        integer       j_getxpos 
        integer       j_getypos 
        integer       j_getparentid 
        logical       j_hasfocus 
        integer       j_getstringwidth 
        integer       j_getfontheight 
        integer       j_getfontascent 
        integer       j_keylistener 
        integer       j_getkeycode 
        integer       j_getkeychar 
        integer       j_mouselistener 
        integer       j_getmousex 
        integer       j_getmousey 
        integer       j_getmousebutton 
        integer       j_focuslistener 
        integer       j_componentlistener 
        integer       j_windowlistener 
        integer       j_getimage 
        integer       j_getscaledimage 
        integer       j_loadimage 
        logical       j_saveimage 
        integer       j_random 



