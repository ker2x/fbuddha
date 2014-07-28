#include <stdio.h>
#include "japi_p.h"


/*

   generated by makejapi4c   DO NOT EDIT

      Tue Feb 25 17:52:36 2003
 

 */


int    j_start( )
{ return( japi_start());  }

int    j_connect( char* arg0)
{ return( japi_connect(arg0));  }

void j_setport( int arg0)
{ japi_setport(arg0);  }

void j_setdebug( int arg0)
{ japi_setdebug(arg0);  }

int  j_frame( char* arg0)
{ return( japi_frame(arg0));  }

int  j_button( int arg0, char* arg1)
{ return( japi_button(arg0, arg1));  }

int  j_graphicbutton( int arg0, char* arg1)
{ return( japi_graphicbutton(arg0, arg1));  }

int  j_checkbox( int arg0, char* arg1)
{ return( japi_checkbox(arg0, arg1));  }

int  j_label( int arg0, char* arg1)
{ return( japi_label(arg0, arg1));  }

int  j_graphiclabel( int arg0, char* arg1)
{ return( japi_graphiclabel(arg0, arg1));  }

int  j_canvas( int arg0, int arg1, int arg2)
{ return( japi_canvas(arg0, arg1, arg2));  }

int  j_panel( int arg0)
{ return( japi_panel(arg0));  }

int  j_borderpanel( int arg0, int arg1)
{ return( japi_borderpanel(arg0, arg1));  }

int  j_radiogroup( int arg0)
{ return( japi_radiogroup(arg0));  }

int  j_radiobutton( int arg0, char* arg1)
{ return( japi_radiobutton(arg0, arg1));  }

int  j_list( int arg0, int arg1)
{ return( japi_list(arg0, arg1));  }

int  j_choice( int arg0)
{ return( japi_choice(arg0));  }

int  j_dialog( int arg0, char* arg1)
{ return( japi_dialog(arg0, arg1));  }

int  j_window( int arg0)
{ return( japi_window(arg0));  }

int  j_popupmenu( int arg0, char* arg1)
{ return( japi_popupmenu(arg0, arg1));  }

int  j_scrollpane( int arg0)
{ return( japi_scrollpane(arg0));  }

int  j_hscrollbar( int arg0)
{ return( japi_hscrollbar(arg0));  }

int  j_vscrollbar( int arg0)
{ return( japi_vscrollbar(arg0));  }

int  j_line( int arg0, int arg1, int arg2, int arg3)
{ return( japi_line(arg0, arg1, arg2, arg3));  }

int  j_printer( int arg0)
{ return( japi_printer(arg0));  }

int  j_image( int arg0, int arg1)
{ return( japi_image(arg0, arg1));  }

char*  j_filedialog( int arg0, char* arg1, char* arg2, char* arg3)
{ return( japi_filedialog(arg0, arg1, arg2, arg3));  }

char*  j_fileselect( int arg0, char* arg1, char* arg2, char* arg3)
{ return( japi_fileselect(arg0, arg1, arg2, arg3));  }

int  j_messagebox( int arg0, char* arg1, char* arg2)
{ return( japi_messagebox(arg0, arg1, arg2));  }

int  j_alertbox( int arg0, char* arg1, char* arg2, char* arg3)
{ return( japi_alertbox(arg0, arg1, arg2, arg3));  }

int  j_choicebox2( int arg0, char* arg1, char* arg2, char* arg3, char* arg4)
{ return( japi_choicebox2(arg0, arg1, arg2, arg3, arg4));  }

int  j_choicebox3( int arg0, char* arg1, char* arg2, char* arg3, char* arg4, char* arg5)
{ return( japi_choicebox3(arg0, arg1, arg2, arg3, arg4, arg5));  }

int  j_progressbar( int arg0, int arg1)
{ return( japi_progressbar(arg0, arg1));  }

int  j_led( int arg0, int arg1, int arg2)
{ return( japi_led(arg0, arg1, arg2));  }

int  j_sevensegment( int arg0, int arg1)
{ return( japi_sevensegment(arg0, arg1));  }

int  j_meter( int arg0, char* arg1)
{ return( japi_meter(arg0, arg1));  }

void j_additem( int arg0, char* arg1)
{ japi_additem(arg0, arg1);  }

int  j_textfield( int arg0, int arg1)
{ return( japi_textfield(arg0, arg1));  }

int  j_textarea( int arg0, int arg1, int arg2)
{ return( japi_textarea(arg0, arg1, arg2));  }

int  j_menubar( int arg0)
{ return( japi_menubar(arg0));  }

int  j_menu( int arg0, char* arg1)
{ return( japi_menu(arg0, arg1));  }

int  j_helpmenu( int arg0, char* arg1)
{ return( japi_helpmenu(arg0, arg1));  }

int  j_menuitem( int arg0, char* arg1)
{ return( japi_menuitem(arg0, arg1));  }

int  j_checkmenuitem( int arg0, char* arg1)
{ return( japi_checkmenuitem(arg0, arg1));  }

void j_pack( int arg0)
{ japi_pack(arg0);  }

void j_print( int arg0)
{ japi_print(arg0);  }

void j_playsoundfile( char* arg0)
{ japi_playsoundfile(arg0);  }

void j_play( int arg0)
{ japi_play(arg0);  }

int  j_sound( char* arg0)
{ return( japi_sound(arg0));  }

void j_setfont( int arg0, int arg1, int arg2, int arg3)
{ japi_setfont(arg0, arg1, arg2, arg3);  }

void j_setfontname( int arg0, int arg1)
{ japi_setfontname(arg0, arg1);  }

void j_setfontsize( int arg0, int arg1)
{ japi_setfontsize(arg0, arg1);  }

void j_setfontstyle( int arg0, int arg1)
{ japi_setfontstyle(arg0, arg1);  }

void j_seperator( int arg0)
{ japi_seperator(arg0);  }

void j_disable( int arg0)
{ japi_disable(arg0);  }

void j_enable( int arg0)
{ japi_enable(arg0);  }

int    j_getstate( int arg0)
{ return( japi_getstate(arg0));  }

int  j_getrows( int arg0)
{ return( japi_getrows(arg0));  }

int  j_getcolumns( int arg0)
{ return( japi_getcolumns(arg0));  }

int  j_getselect( int arg0)
{ return( japi_getselect(arg0));  }

int    j_isselect( int arg0, int arg1)
{ return( japi_isselect(arg0, arg1));  }

int    j_isvisible( int arg0)
{ return( japi_isvisible(arg0));  }

int    j_isparent( int arg0, int arg1)
{ return( japi_isparent(arg0, arg1));  }

int    j_isresizable( int arg0)
{ return( japi_isresizable(arg0));  }

void j_select( int arg0, int arg1)
{ japi_select(arg0, arg1);  }

void j_deselect( int arg0, int arg1)
{ japi_deselect(arg0, arg1);  }

void j_multiplemode( int arg0, int arg1)
{ japi_multiplemode(arg0, arg1);  }

void j_insert( int arg0, int arg1, char* arg2)
{ japi_insert(arg0, arg1, arg2);  }

void j_remove( int arg0, int arg1)
{ japi_remove(arg0, arg1);  }

void j_removeitem( int arg0, char* arg1)
{ japi_removeitem(arg0, arg1);  }

void j_removeall( int arg0)
{ japi_removeall(arg0);  }

void j_setstate( int arg0, int arg1)
{ japi_setstate(arg0, arg1);  }

void j_setrows( int arg0, int arg1)
{ japi_setrows(arg0, arg1);  }

void j_setcolumns( int arg0, int arg1)
{ japi_setcolumns(arg0, arg1);  }

void j_seticon( int arg0, int arg1)
{ japi_seticon(arg0, arg1);  }

void j_setimage( int arg0, int arg1)
{ japi_setimage(arg0, arg1);  }

void j_setvalue( int arg0, int arg1)
{ japi_setvalue(arg0, arg1);  }

void j_setradiogroup( int arg0, int arg1)
{ japi_setradiogroup(arg0, arg1);  }

void j_setunitinc( int arg0, int arg1)
{ japi_setunitinc(arg0, arg1);  }

void j_setblockinc( int arg0, int arg1)
{ japi_setblockinc(arg0, arg1);  }

void j_setmin( int arg0, int arg1)
{ japi_setmin(arg0, arg1);  }

void j_setmax( int arg0, int arg1)
{ japi_setmax(arg0, arg1);  }

void j_setdanger( int arg0, int arg1)
{ japi_setdanger(arg0, arg1);  }

void j_setslidesize( int arg0, int arg1)
{ japi_setslidesize(arg0, arg1);  }

void j_setcursor( int arg0, int arg1)
{ japi_setcursor(arg0, arg1);  }

void j_setresizable( int arg0, int arg1)
{ japi_setresizable(arg0, arg1);  }

int  j_getlength( int arg0)
{ return( japi_getlength(arg0));  }

int  j_getvalue( int arg0)
{ return( japi_getvalue(arg0));  }

int  j_getdanger( int arg0)
{ return( japi_getdanger(arg0));  }

int  j_getscreenheight( )
{ return( japi_getscreenheight());  }

int  j_getscreenwidth( )
{ return( japi_getscreenwidth());  }

int  j_getheight( int arg0)
{ return( japi_getheight(arg0));  }

int  j_getwidth( int arg0)
{ return( japi_getwidth(arg0));  }

int  j_getinsets( int arg0, int arg1)
{ return( japi_getinsets(arg0, arg1));  }

int  j_getlayoutid( int arg0)
{ return( japi_getlayoutid(arg0));  }

int  j_getinheight( int arg0)
{ return( japi_getinheight(arg0));  }

int  j_getinwidth( int arg0)
{ return( japi_getinwidth(arg0));  }

char*  j_gettext( int arg0, char* arg1)
{ return( japi_gettext(arg0, arg1));  }

char*  j_getitem( int arg0, int arg1, char* arg2)
{ return( japi_getitem(arg0, arg1, arg2));  }

int  j_getitemcount( int arg0)
{ return( japi_getitemcount(arg0));  }

void j_delete( int arg0, int arg1, int arg2)
{ japi_delete(arg0, arg1, arg2);  }

void j_replacetext( int arg0, char* arg1, int arg2, int arg3)
{ japi_replacetext(arg0, arg1, arg2, arg3);  }

void j_appendtext( int arg0, char* arg1)
{ japi_appendtext(arg0, arg1);  }

void j_inserttext( int arg0, char* arg1, int arg2)
{ japi_inserttext(arg0, arg1, arg2);  }

void j_settext( int arg0, char* arg1)
{ japi_settext(arg0, arg1);  }

void j_selectall( int arg0)
{ japi_selectall(arg0);  }

void j_selecttext( int arg0, int arg1, int arg2)
{ japi_selecttext(arg0, arg1, arg2);  }

int  j_getselstart( int arg0)
{ return( japi_getselstart(arg0));  }

int  j_getselend( int arg0)
{ return( japi_getselend(arg0));  }

char*  j_getseltext( int arg0, char* arg1)
{ return( japi_getseltext(arg0, arg1));  }

int  j_getcurpos( int arg0)
{ return( japi_getcurpos(arg0));  }

void j_setcurpos( int arg0, int arg1)
{ japi_setcurpos(arg0, arg1);  }

void j_setechochar( int arg0, char arg1)
{ japi_setechochar(arg0, arg1);  }

void j_seteditable( int arg0, int arg1)
{ japi_seteditable(arg0, arg1);  }

void j_setshortcut( int arg0, char arg1)
{ japi_setshortcut(arg0, arg1);  }

void j_quit( )
{ japi_quit();  }

void j_kill( )
{ japi_kill();  }

void j_setsize( int arg0, int arg1, int arg2)
{ japi_setsize(arg0, arg1, arg2);  }

int  j_getaction( )
{ return( japi_getaction());  }

int  j_nextaction( )
{ return( japi_nextaction());  }

void j_show( int arg0)
{ japi_show(arg0);  }

void j_showpopup( int arg0, int arg1, int arg2)
{ japi_showpopup(arg0, arg1, arg2);  }

void j_add( int arg0, int arg1)
{ japi_add(arg0, arg1);  }

void j_release( int arg0)
{ japi_release(arg0);  }

void j_releaseall( int arg0)
{ japi_releaseall(arg0);  }

void j_hide( int arg0)
{ japi_hide(arg0);  }

void j_dispose( int arg0)
{ japi_dispose(arg0);  }

void j_setpos( int arg0, int arg1, int arg2)
{ japi_setpos(arg0, arg1, arg2);  }

int  j_getviewportheight( int arg0)
{ return( japi_getviewportheight(arg0));  }

int  j_getviewportwidth( int arg0)
{ return( japi_getviewportwidth(arg0));  }

int  j_getxpos( int arg0)
{ return( japi_getxpos(arg0));  }

int  j_getypos( int arg0)
{ return( japi_getypos(arg0));  }

void j_getpos( int arg0, int* arg1, int* arg2)
{ japi_getpos(arg0, arg1, arg2);  }

int  j_getparentid( int arg0)
{ return( japi_getparentid(arg0));  }

void j_setfocus( int arg0)
{ japi_setfocus(arg0);  }

int    j_hasfocus( int arg0)
{ return( japi_hasfocus(arg0));  }

int  j_getstringwidth( int arg0, char* arg1)
{ return( japi_getstringwidth(arg0, arg1));  }

int  j_getfontheight( int arg0)
{ return( japi_getfontheight(arg0));  }

int  j_getfontascent( int arg0)
{ return( japi_getfontascent(arg0));  }

int  j_keylistener( int arg0)
{ return( japi_keylistener(arg0));  }

int  j_getkeycode( int arg0)
{ return( japi_getkeycode(arg0));  }

int  j_getkeychar( int arg0)
{ return( japi_getkeychar(arg0));  }

int  j_mouselistener( int arg0, int arg1)
{ return( japi_mouselistener(arg0, arg1));  }

int  j_getmousex( int arg0)
{ return( japi_getmousex(arg0));  }

int  j_getmousey( int arg0)
{ return( japi_getmousey(arg0));  }

void j_getmousepos( int arg0, int* arg1, int* arg2)
{ japi_getmousepos(arg0, arg1, arg2);  }

int  j_getmousebutton( int arg0)
{ return( japi_getmousebutton(arg0));  }

int  j_focuslistener( int arg0)
{ return( japi_focuslistener(arg0));  }

int  j_componentlistener( int arg0, int arg1)
{ return( japi_componentlistener(arg0, arg1));  }

int  j_windowlistener( int arg0, int arg1)
{ return( japi_windowlistener(arg0, arg1));  }

void j_setflowlayout( int arg0, int arg1)
{ japi_setflowlayout(arg0, arg1);  }

void j_setborderlayout( int arg0)
{ japi_setborderlayout(arg0);  }

void j_setgridlayout( int arg0, int arg1, int arg2)
{ japi_setgridlayout(arg0, arg1, arg2);  }

void j_setfixlayout( int arg0)
{ japi_setfixlayout(arg0);  }

void j_setnolayout( int arg0)
{ japi_setnolayout(arg0);  }

void j_setborderpos( int arg0, int arg1)
{ japi_setborderpos(arg0, arg1);  }

void j_sethgap( int arg0, int arg1)
{ japi_sethgap(arg0, arg1);  }

void j_setvgap( int arg0, int arg1)
{ japi_setvgap(arg0, arg1);  }

void j_setinsets( int arg0, int arg1, int arg2, int arg3, int arg4)
{ japi_setinsets(arg0, arg1, arg2, arg3, arg4);  }

void j_setalign( int arg0, int arg1)
{ japi_setalign(arg0, arg1);  }

void j_setflowfill( int arg0, int arg1)
{ japi_setflowfill(arg0, arg1);  }

void j_translate( int arg0, int arg1, int arg2)
{ japi_translate(arg0, arg1, arg2);  }

void j_cliprect( int arg0, int arg1, int arg2, int arg3, int arg4)
{ japi_cliprect(arg0, arg1, arg2, arg3, arg4);  }

void j_drawrect( int arg0, int arg1, int arg2, int arg3, int arg4)
{ japi_drawrect(arg0, arg1, arg2, arg3, arg4);  }

void j_fillrect( int arg0, int arg1, int arg2, int arg3, int arg4)
{ japi_fillrect(arg0, arg1, arg2, arg3, arg4);  }

void j_drawroundrect( int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6)
{ japi_drawroundrect(arg0, arg1, arg2, arg3, arg4, arg5, arg6);  }

void j_fillroundrect( int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6)
{ japi_fillroundrect(arg0, arg1, arg2, arg3, arg4, arg5, arg6);  }

void j_drawoval( int arg0, int arg1, int arg2, int arg3, int arg4)
{ japi_drawoval(arg0, arg1, arg2, arg3, arg4);  }

void j_filloval( int arg0, int arg1, int arg2, int arg3, int arg4)
{ japi_filloval(arg0, arg1, arg2, arg3, arg4);  }

void j_drawcircle( int arg0, int arg1, int arg2, int arg3)
{ japi_drawcircle(arg0, arg1, arg2, arg3);  }

void j_fillcircle( int arg0, int arg1, int arg2, int arg3)
{ japi_fillcircle(arg0, arg1, arg2, arg3);  }

void j_drawarc( int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6)
{ japi_drawarc(arg0, arg1, arg2, arg3, arg4, arg5, arg6);  }

void j_fillarc( int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6)
{ japi_fillarc(arg0, arg1, arg2, arg3, arg4, arg5, arg6);  }

void j_drawline( int arg0, int arg1, int arg2, int arg3, int arg4)
{ japi_drawline(arg0, arg1, arg2, arg3, arg4);  }

void j_drawpolyline( int arg0, int arg1, int* arg2, int* arg3)
{ japi_drawpolyline(arg0, arg1, arg2, arg3);  }

void j_drawpolygon( int arg0, int arg1, int* arg2, int* arg3)
{ japi_drawpolygon(arg0, arg1, arg2, arg3);  }

void j_fillpolygon( int arg0, int arg1, int* arg2, int* arg3)
{ japi_fillpolygon(arg0, arg1, arg2, arg3);  }

void j_drawpixel( int arg0, int arg1, int arg2)
{ japi_drawpixel(arg0, arg1, arg2);  }

void j_drawstring( int arg0, int arg1, int arg2, char* arg3)
{ japi_drawstring(arg0, arg1, arg2, arg3);  }

void j_setxor( int arg0, int arg1)
{ japi_setxor(arg0, arg1);  }

int  j_getimage( int arg0)
{ return( japi_getimage(arg0));  }

void j_getimagesource( int arg0, int arg1, int arg2, int arg3, int arg4, int* arg5, int* arg6, int* arg7)
{ japi_getimagesource(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);  }

void j_drawimagesource( int arg0, int arg1, int arg2, int arg3, int arg4, int* arg5, int* arg6, int* arg7)
{ japi_drawimagesource(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);  }

int  j_getscaledimage( int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6)
{ return( japi_getscaledimage(arg0, arg1, arg2, arg3, arg4, arg5, arg6));  }

void j_drawimage( int arg0, int arg1, int arg2, int arg3)
{ japi_drawimage(arg0, arg1, arg2, arg3);  }

void j_drawscaledimage( int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8, int arg9)
{ japi_drawscaledimage(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);  }

void j_setcolor( int arg0, int arg1, int arg2, int arg3)
{ japi_setcolor(arg0, arg1, arg2, arg3);  }

void j_setcolorbg( int arg0, int arg1, int arg2, int arg3)
{ japi_setcolorbg(arg0, arg1, arg2, arg3);  }

void j_setnamedcolor( int arg0, int arg1)
{ japi_setnamedcolor(arg0, arg1);  }

void j_setnamedcolorbg( int arg0, int arg1)
{ japi_setnamedcolorbg(arg0, arg1);  }

int  j_loadimage( char* arg0)
{ return( japi_loadimage(arg0));  }

int    j_saveimage( int arg0, char* arg1, int arg2)
{ return( japi_saveimage(arg0, arg1, arg2));  }

void j_sync( )
{ japi_sync();  }

void j_beep( )
{ japi_beep();  }

int  j_random( )
{ return( japi_random());  }

void j_sleep( int arg0)
{ japi_sleep(arg0);  }

