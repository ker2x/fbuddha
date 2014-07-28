C       Example mandel.f 

        INCLUDE "japi.f"


        integer breite,hoehe
        integer x,y,do_work,it
        real zre,zim
        real xstart,ystart,xend,yend
        
        integer frame,menubar
        integer file,calc
        integer quit,start,stop
        integer canvas,obj
                
        xstart = -1.8
        xend   =  0.8
        ystart = -1.0
        yend   =  1.0

        hoehe  = 240
        breite = 320
                
        call j_setdebug(-3)
        
        if( .not. j_start()) then
            write(*,*) "can't connect to server"   
            goto 20
        endif
        
        frame   = j_frame("Mandelbrot")         
        menubar = j_menubar(frame)
        file    = j_menu(menubar,"File")
        calc    = j_menu(menubar,"Calc")
        quit    = j_menuitem(file,"Quit")
        start   = j_menuitem(calc,"Start")
        stop    = j_menuitem(calc,"Stop")

        canvas  = j_canvas(frame,breite,hoehe)
        call j_setpos(canvas,10,60)
        call j_pack(frame)
        call j_show(frame)

        x=-1
        y=-1
        do_work=0
10      continue
                
            if(do_work.eq.1) then
                obj=j_getaction()
            else
                obj=j_nextaction()
            endif
                               
            if(obj.eq.quit) goto 20
        
            if(obj.eq.start) then
                x=-1
                y=-1
                do_work=1
            endif
        
            if(obj.eq.stop) do_work=0
                        
            if(do_work.eq.1) then
                x=mod((x+1),breite)
                if(x.eq.0) y = mod((y+1),hoehe)
                if((x.eq.breite-1).and.(y.eq.hoehe-1)) then
                    do_work=0
                else
                    zre = xstart + x*(xend-xstart)/breite
                    zim = ystart + y*(yend-ystart)/hoehe
                    it = mandel(zre,zim,512)
c                    r=mod(it*11,256)
c                    g=mod(it*13,256)
c                    b=mod(it*17,256)
c                    call j_setcolor(canvas,r,g,b)
                    call j_setcolor(canvas,it*11,it*13,it*17)
                    call j_drawpixel(canvas,x,y)
                endif
            endif                                

        goto 10

20      continue
        call j_quit()        
        end


        integer function  mandel(zre,zim,maxiter)
        real zre,zim
        integer maxiter

            real x,y,tmp,betrag
            integer iter

            x=0.0
            y=0.0
            iter=0
            betrag=0.0
    
10          continue        
  
            tmp = x*x-y*y+zre
            y = 2*x*y+zim
            x = tmp
            betrag = x*x + y*y
            iter = iter + 1
        
            if(betrag.gt.4.0) goto 20
            if(iter.lt.maxiter)  goto 10
        
20          mandel=iter    
        end               

