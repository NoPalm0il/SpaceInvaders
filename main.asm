setupscreen macro								; setup video mode
    xor		ah, ah								; graph mode ah:=0
    mov		al, 04h								; 320x200 color mode
    int		10h
; hide cursor
    mov		ch, 32								; sets the cursor size
    mov		ah, 1								; cursor size service
    int		10h
; palete de cores
    mov		ax, 0dh								; color palette for display mode
    xor		bl, bl								; color or palette(0) to be used with color id
    int		10h
endm

normalscreen macro								; sets the normal video mode
    xor     ah, ah
    mov		al, 03h								; normal video mode (text)
    int		10h
endm

paintscoreline macro
    mov 	cx, 240
    mov		dx, 200
paintlineloop:
    mov		al, 111b
    mov		ah, 0ch
    int		10h
    dec		dx
    cmp		dx, 0
    jnz		paintlineloop
endm

stack segment para stack
    db 64 dup ('mystack')
stack ends

data segment para 'data'
    seed 				dw 	9371h

    p1xpos				dw	50					; p1 x pos
    p1ypos				dw	150					; p1 y pos
    isp1alive			db	1					; is player 1 alive? 1 = true
    lasersp1_x			db	20 dup (?)
    lasersp1_y			db	20 dup (?)
    totalp1lasers		db	0

    p2xpos				dw	150					; p2 x pos
    p2ypos				dw	150					; p2 y pos
    isp2alive			db	1					; is player 2 alive? 1 = true
    lasersp2_x			db	20 dup (?)
    lasersp2_y			db	20 dup (?)
    totalp2lasers		db	0

    enemyship_xpos		dw	0h,0h,0h,0h,0h
    enemyship_ypos		dw	14h,20h,0h,10h,0bh
    enemyship_index     dw  0

    outputfile 			db	"printscreen.ppm",0
    outhandle 			dw	?

    scoremessage		db	"SCORE: $"
    score				dw	0

    isgamepaused		db	0
    isgamealive			db	1

    totaltime           dw  500
    time                db  0
data ends

code segment para 'code'

; ############################# MAIN PROC #############################

main proc far
    assume cs:code,ds:data,es:data,ss:stack
    push	ds
    xor		ax, ax
    push	ax
    mov 	ax,data
    mov 	ds,ax
    mov 	es,ax

    setupscreen								; macro to setup the video mode
    paintscoreline
    call	setupenemiescords				; macro for setting up enemies coords
    call	printscorestring_at_pos
    call	paintscore

    ;xor     ah, ah
    ;int     33h
    ;cmp     ax, 0ffffh
    ;jne     callgameloop
    ;mov     ah, 04
    ;mov     cx, p2xpos
    ;mov     dx, p2ypos
    ;int     33h
callgameloop:
    call	gameloop						; main game loop

    normalscreen							; macro to setup normal text mode

    ret
main endp

gameloop proc near

    xor     ax, ax
    xor     bx, bx
    xor     cx, cx
    xor     dx, dx
    mov		al, 01h							; player color
    call	paintplayer1
    mov		al, 02h							; player color
    call	paintplayer2
    call 	paintenemies

mainloop:									; ################## MAIN LOOP ##################

    call 	delay                           ; delays the main loop
    call    timeproc                        ; checks game time
    call	paintenemies                    ; paints all the enemies
    call 	readchar                        ; reads the keyboard buffer
    ;call    readmouse
    call	drawlasersp1                    ; draw the lasers from player 1
    call	drawlasersp2                    ; draw the lasers from player 2

    mov     ah, isp1alive
    mov     al, isp2alive
    or      al, ah                          ; or p1alive p2alive, 0 || 0 = 0 ends the game
    cmp     al, 0
    je      endgameloop
    mov     al, isgamealive
    cmp		isgamealive, 1
    je		mainloop
endgameloop:
    ret
gameloop endp

; Procedure to setup "random" x coords for all the enemies
setupenemiescords proc near
    mov		cx, 4
    lea		bx, enemyship_xpos
    mov		dx, 30
loopcoords:
    push	bx
    call 	random
    mov		ax, seed
    mov		bx, 180
    xor		dx, dx
    div		bx
    pop		bx
    mov		[bx], dx
    inc		bx
    loop	loopcoords
setupenemiescords endp

; ############################# READ CHAR #############################

; Listens the keyboard, not waiting for input
readchar proc near
    mov		ah, 01h
    int		16h
    jnz		keybdpressed
    xor		dl, dl
    ret
keybdpressed:
    xor		ah, ah
    int		16h								; extract the keystroke from the buffer, clears zf and buffer
    call	keyboard_keys
    ret
readchar endp

readmouse proc near

    mov     ah, 03h
    int     33h

    mov     p2xpos, cx
    mov     p2ypos, dx

    call    removeplayer2color
    mov     al, 010b
    call    paintplayer2

    ret
readmouse endp
; keys function
keyboard_keys proc near
    cmp		al,"w"
    je		p1up							; player 1 up
    cmp		al,"a"
    je		p1lf							; player 1 left
    cmp		al,"s"
    je		p1dw							; player 1 down
    cmp		al,"d"
    je		p1rt							; player 1 right
    cmp		al,"x"
    je		shootp1

    cmp		al,"i"
    je		p2up							; player 2 up
    cmp		al,"j"
    je		p2lf							; player 2 left
    cmp		al,"k"
    je		p2dw							; player 2 down
    cmp		al,"l"
    je		p2rt							; player 2 right
    cmp		al,"n"
    je		shootp2

    cmp		al,"g"
    je		saveimg
    cmp		al,"q"
    je		quit_game

    jmp		exit
p1up:
    call	p1upproc
    jmp		paintp1
p1dw:
    call	p1dwproc
    jmp		paintp1
p1lf:
    call	p1lfproc
    jmp		paintp1
p1rt:
    call	p1rtproc
    jmp		paintp1
paintp1:
    mov		al, 01h							; player color
    cmp     isp1alive, 0
    je      exit
    call	paintplayer1
    jmp		exit
shootp1:
    cmp     isp1alive, 0
    je      exit2
    call	firelaserplayer1
    jmp		exit
saveimg:
    call 	savescreen
    jmp		exit
quit_game:
    xor		ah, ah
    mov		isgamealive, ah
exit:
    ret
p2up:
    call	p2upproc
    jmp		paintp2
p2lf:
    call	p2lfproc
    jmp		paintp2
p2dw:
    call	p2dwproc
    jmp		paintp2
p2rt:
    call	p2rtproc
    jmp		paintp2
paintp2:
    mov		al, 02h							; player color
    cmp     isp2alive, 0
    je      exit2
    call 	paintplayer2
    jmp 	exit2
shootp2:
    cmp     isp2alive, 0
    je      exit2
    call	firelaserplayer2
exit2:
    ret
keyboard_keys endp

; ################################################################## KEYBOARD
p1upproc proc near
    cmp     isp1alive, 0
    je      endp1upproc
    call	removeplayer1color
    dec		p1ypos
    dec		p1ypos
    dec		p1ypos
    cmp		p1ypos, 135
    ja		endp1upproc
    inc		p1ypos
    inc		p1ypos
    inc		p1ypos
endp1upproc:
    ret
p1upproc endp
p1dwproc proc near
    cmp     isp1alive, 0
    je      endp1dwproc
    call	removeplayer1color
    inc		p1ypos
    inc		p1ypos
    inc		p1ypos
    cmp		p1ypos, 196
    jb		endp1dwproc
    dec		p1ypos
    dec		p1ypos
    dec		p1ypos
endp1dwproc:
    ret
p1dwproc endp
p1lfproc proc near
    cmp     isp1alive, 0
    je      endp1lfproc
    call	removeplayer1color
    dec		p1xpos
    dec		p1xpos
    dec		p1xpos
    cmp		p1xpos, 4
    ja		endp1lfproc
    inc		p1xpos
    inc		p1xpos
    inc		p1xpos
endp1lfproc:
    ret
p1lfproc endp
p1rtproc proc near
    cmp     isp1alive, 0
    je      endp1rtproc
    call	removeplayer1color
    inc		p1xpos
    inc		p1xpos
    inc		p1xpos
    cmp     p1xpos, 236
    jb      endp1rtproc
    dec     p1xpos
    dec     p1xpos
    dec     p1xpos
endp1rtproc:
    ret
p1rtproc endp

p2upproc proc near
    cmp     isp2alive, 0
    je      endp2upproc
    call	removeplayer2color
    dec		p2ypos
    dec		p2ypos
    dec		p2ypos
    cmp		p2ypos, 135
    ja		endp2upproc
    inc		p2ypos
    inc		p2ypos
    inc		p2ypos
endp2upproc:
    ret
p2upproc endp
p2dwproc proc near
    cmp     isp2alive, 0
    je      endp2dwproc
    call	removeplayer2color
    inc		p2ypos
    inc		p2ypos
    inc		p2ypos
    cmp		p2ypos, 196
    jb		endp2dwproc
    dec		p2ypos
    dec		p2ypos
    dec		p2ypos
endp2dwproc:
    ret
p2dwproc endp
p2lfproc proc near
    cmp     isp2alive, 0
    je      endp2lfproc
    call	removeplayer2color
    dec		p2xpos
    dec		p2xpos
    dec		p2xpos
    cmp		p2xpos, 4
    ja		endp2lfproc
    inc		p2xpos
    inc		p2xpos
    inc		p2xpos
endp2lfproc:
    ret
p2lfproc endp
p2rtproc proc near
    cmp     isp2alive, 0
    je      endp2rtproc
    call	removeplayer2color
    inc		p2xpos
    inc		p2xpos
    inc		p2xpos
    cmp		p2xpos, 236
    jb		endp2rtproc
    dec		p2xpos
    dec		p2xpos
    dec		p2xpos
endp2rtproc:
    ret
p2rtproc endp

; ################################################################## KEYBOARD


; ############################# SET CURSOR POS #############################

printscorestring_at_pos proc near

    mov		ah, 02h							; set cursor position service
    xor		bh, bh							; display page number
    mov		dh, 2							; row
    mov		dl, 31							; column
    int		10h

    mov		ah, 09h
    lea		dx, scoremessage
    int		21h

    ret
printscorestring_at_pos endp

; ############################# REMOVE PLAYER 1 PROC #############################

removeplayer1color proc near
    push	ax
    mov		al, 00							; paint black
    call 	paintplayer1
    pop		ax
    ret
removeplayer1color endp

; ############################# REMOVE PLAYER 2 PROC #############################

removeplayer2color proc near
    push	ax
    mov		al, 00							; paint black
    call 	paintplayer2
    pop		ax
    ret
removeplayer2color endp

; ############################# PAINT PLAYER 1 PROC #############################

; al = color
paintplayer1 proc near
    push	bx
    push	cx
    push	dx

    mov		ah, 0ch
    mov		cx, p1xpos						; horizontal pos
    mov		dx, p1ypos						; vert pos
    mov     bl, 0                           ; 0 is p1 that is being painted

    call 	paintplayerbox

    pop		dx
    pop		cx
    pop		bx

    ret
paintplayer1 endp

; ############################# PAINT PLAYER 2 PROC #############################

; al = color
paintplayer2 proc near
    push	bx
    push	cx
    push	dx

    mov		ah, 0ch
    mov		cx, p2xpos						; horizontal pos
    mov		dx, p2ypos						; vert pos
    mov     bl, 1                           ; 1 is p2 that is being painted

    call 	paintplayerbox

    pop		dx
    pop		cx
    pop		bx

    ret
paintplayer2 endp

; ############################# PAINT ENEMIES #############################

paintenemies proc near
    xor     ax, ax
    xor     bx, bx
    xor     cx, cx
    xor     dx, dx

    lea		bx, enemyship_xpos
    lea		ax, enemyship_ypos
    mov		cx, 5

paintblackenemyloop:						; ### PAINTS BLACK ###
    push	cx	; +1 PUSH
    mov		cx, [bx]						; load enemy x pos
    xor		ch, ch
    ;--
    push	bx	; +1 PUSH bx has xcoords array and index
    mov		bx, ax
    mov		dx, [bx]
    xor		dh, dh
    inc		bx
    mov		ax, bx
    pop		bx
    ;--
    push	ax
    mov		ah, 0ch
    mov		al, 0							; black color
    call 	paintenemybox
    pop		ax

    inc		bx
    inc     enemyship_index
    pop		cx	; -1 POP
    loop paintblackenemyloop

    lea		bx, enemyship_ypos
    mov		cx, 5
enemy_discent:
    mov		ax, [bx]
    inc		ax
    mov		[bx], ax
    inc		bx
    loop	enemy_discent

    lea		bx, enemyship_xpos
    lea		ax, enemyship_ypos
    mov		cx, 5
    mov     enemyship_index, 0
paintenemyloop:								; ### PAINTS COLOR ###
    push	cx	; +1 PUSH
    mov		cx, [bx]						; load enemy x pos
    xor		ch, ch
    ;--
    push	bx	; +1 PUSH bx has xcoords array and index
    mov		bx, ax
    mov		dx, [bx]
    xor		dh, dh
    inc		bx
    mov		ax, bx
    pop		bx
    ;--
    push	ax
    mov		ah, 0ch
    mov		al, 100b						; red color
    call 	paintenemybox
    pop		ax

    inc		bx
    inc     enemyship_index
    pop		cx	; -1 POP
    loop	paintenemyloop

    ret
paintenemies endp

; ############################# PAINT PLAYER BOX #############################

paintplayerbox proc near
    sub		cx, 4							; top left corner
    sub		dx, 3							; top left corner
    push	bx
    xor		bx, bx							; 9 pixels right

paintyplayer:
    cmp		bl, 7
    je		endplayerloop
    xor		bh, bh
paintxplayer:
    cmp		bh, 9
    je      continueplayer
    push	bx
    push    ax
    mov     ah, 0dh
    xor		bh, bh
    int		10h
    cmp     al, 100b
    je      playerpixelcollision
    pop     ax
    xor     bh, bh
    int     10h
    pop		bx
    inc		cx
    inc		bh
    jmp		paintxplayer
continueplayer:
    inc		bl
    inc		dx
    sub		cx, 9
    jmp		paintyplayer
playerpixelcollision:
    pop     ax
    pop     ax
    pop     bx                              ; bl contains wich player is being painted
    call    playercollision
    ret
    
endplayerloop:
    pop		bx
    ret
paintplayerbox endp


playercollision proc near
    cmp     bl, 0                           ; is p1
    je      paintingp1
    cmp     bl, 1
    je      paintingp2
paintingp1:
    mov     isp1alive, 0
    mov     cx, p1xpos
    mov     dx, p1ypos
    jmp     continuepainting
paintingp2:
    mov     isp2alive, 0
    mov     cx, p2xpos
    mov     dx, p2ypos

continuepainting:
    xor     bx, bx
    xor     al, al
    mov     ah, 0ch
    sub		cx, 4							; top left corner
    sub		dx, 3							; top left corner

paintyplayerblack:
    cmp		bl, 7
    je		endplayerloopblack
    xor		bh, bh
paintxplayerblack:
    cmp		bh, 9
    je      continueplayerblack
    push	bx
    xor     bh, bh
    int     10h
    pop		bx
    inc		cx
    inc		bh
    jmp		paintxplayerblack
continueplayerblack:
    inc		bl
    inc		dx
    sub		cx, 9
    jmp		paintyplayerblack
  
endplayerloopblack:
    ret
playercollision endp

; al = color
; cx = x
; dx = y
paintenemybox proc near
    push	cx
    push	dx
    push	bx								; bx still has the index to the array

    sub		cx, 3							; top left corner (cx = x)
    sub		dx, 3							; top left corner (dx = y)
    xor		bx, bx
paintyenemy:
    cmp		bl, 7
    je		endenemyloop
    xor		bh, bh
paintxenemy:
    cmp		bh, 7
    je		continueenemy
    ; inside "for" loop
    push	bx	; +1 PUSH
    xor		bh, bh
    push	ax	; +1 PUSH
    mov		ah, 0dh							; checks pixel color for colision
    int		10h
    cmp		al, 010b
    je		pixelcollision
    cmp		al, 001b
    je		pixelcollision
    mov		ah, 0ch
    pop		ax	; -1 POP
    int		10h								; bh must be 0 (screen 0)
    pop		bx	; -1 POP

    inc		cx
    inc		bh
    jmp		paintxenemy
continueenemy:								; 251
    inc		bl
    inc		dx
    sub		cx, 7
    jmp		paintyenemy

pixelcollision:
    pop		ax	; -1 POP
    pop		dx	; -1 POP
    pop		bx
    pop     dx
    pop     cx
    push	bx								; bx stays with the array and index
    call	enemyshipcollision
    ret
endenemyloop:
    pop		bx
    pop		dx
    pop		cx
    ret
paintenemybox endp

; cx = x
; dx = y
paintenemyboxblack proc near
    push	cx
    push	dx
    push	bx								; bx still has the index to the array

    xor     al, al
    
    sub		cx, 3							; top left corner (cx = x)
    sub		dx, 3							; top left corner (dx = y)
    xor		bx, bx
paintyenemyblack:
    cmp		bl, 7
    je		endenemyloopblack
    xor		bh, bh
paintxenemyblack:
    cmp		bh, 7
    je		continueenemyblack
    ; inside "for" loop
    push	bx	; +1 PUSH
    xor		bh, bh
    push	ax	; +1 PUSH
    mov		ah, 0dh							; checks pixel color for colision
    int		10h
    pop		ax	; -1 POP
    int		10h								; bh must be 0 (screen 0)
    pop		bx	; -1 POP

    inc		cx
    inc		bh
    jmp		paintxenemyblack
continueenemyblack:								; 251
    inc		bl
    inc		dx
    sub		cx, 7
    jmp		paintyenemyblack

endenemyloopblack:
    pop		bx
    pop		dx
    pop		cx
    ret
paintenemyboxblack endp

enemyshipcollision proc near
    push    ax
    push    bx

    call paintenemyboxblack
gennewxcoord:
    call    random

    xor     dx, dx
    mov     cx, 190
    mov     ax, seed
    div     cx
    cmp     dx, 20
    jb      gennewxcoord
    cmp     dx, 200
    ja      gennewxcoord

    mov     [bx], dx

    lea     bx, enemyship_ypos
    mov     dx, 0FFFCh

    ;mov		[enemyship_index + bx], dx       ; bx has enemy x coords array and index position pointer

    inc		score
    call	paintscore
    
    pop     bx
    pop     ax
    ret
enemyshipcollision endp


paintscore proc near
    push    bx

    mov		ah, 02h							; set cursor position service
    xor		bh, bh							; display page number
    mov		dh, 3							; row
    mov		dl, 32							; column
    int		10h

    mov		ax, score

    call	dispx
    xor		ax, ax

    pop     bx
    ret
paintscore endp

drawlasersp1 proc near
    xor		ax, ax
    xor     bx, bx
    xor     cx, cx
    xor     dx, dx
    cmp		al, totalp1lasers				; al := 0
    jne		paintblacklasersp1
    ret
paintblacklasersp1:							; 002c4H
    lea		bx, lasersp1_x
    add		bx, ax
    mov		cx, [bx]
    xor		ch, ch
    lea		bx, lasersp1_y
    add		bx, ax
    mov		dx, [bx]
    xor		dh, dh
    push	ax
    xor		bx, bx
paintblackvertl1:
    mov		ah, 0ch
    mov		al, 0
    int		10h
    dec		dx
    inc		bl
    cmp		bl, 6							; paint 6 pixels vertical
    jb		paintblackvertl1

    pop		ax
    inc		al
    cmp		al, totalp1lasers
    jb		paintblacklasersp1	; end loop

    xor		ch, ch
    mov		cl, totalp1lasers
    lea		bx, lasersp1_y
decp1lasersy:
    mov		ax, [bx]
    dec		ax
    dec		ax
    mov		[bx], ax
    inc		bx
    loop	decp1lasersy

    xor		ax, ax
paintlasersp1:
    lea		bx, lasersp1_x
    add		bx, ax
    mov		cx, [bx]
    xor		ch, ch
    lea		bx, lasersp1_y
    add		bx, ax
    mov		dx, [bx]
    xor		dh, dh
    push	ax

    xor		bx, bx
paintvertl1:
    mov		ah, 0ch
    mov		al, 001b
    int		10h
    dec		dx
    inc		bl
    cmp		bl, 6							; paint 6 pixels vertical
    jb		paintvertl1

    pop		ax
    inc		al
    cmp		al, totalp1lasers
    jb		paintlasersp1

    ret
drawlasersp1 endp

drawlasersp2 proc near
    xor		ax, ax
    xor     bx, bx
    xor     cx, cx
    xor     dx, dx
    cmp		al, totalp2lasers				; al := 0
    jne		paintblacklasersp2
    ret
paintblacklasersp2:							; 002c4H
    lea		bx, lasersp2_x
    add		bx, ax
    mov		cx, [bx]
    xor		ch, ch
    lea		bx, lasersp2_y
    add		bx, ax
    mov		dx, [bx]
    xor		dh, dh
    push	ax
    xor		bx, bx
paintblackvertl2:
    mov		ah, 0ch
    mov		al, 0
    int		10h
    dec		dx
    inc		bl
    cmp		bl, 6							; paint 6 pixels vertical
    jb		paintblackvertl2

    pop		ax
    inc		al
    cmp		al, totalp2lasers
    jb		paintblacklasersp2	; end loop

    xor		ch, ch
    mov		cl, totalp2lasers
    lea		bx, lasersp2_y
decp2lasersy:
    mov		ax, [bx]
    dec		ax
    dec		ax
    mov		[bx], ax
    inc		bx
    loop	decp2lasersy

    xor		ax, ax
paintlasersp2:
    lea		bx, lasersp2_x
    add		bx, ax
    mov		cx, [bx]
    xor		ch, ch
    lea		bx, lasersp2_y
    add		bx, ax
    mov		dx, [bx]
    xor		dh, dh
    push	ax

    xor		bx, bx
paintvertl2:
    mov		ah, 0ch
    mov		al, 010b
    int		10h
    dec		dx
    inc		bl
    cmp		bl, 6							; paint 6 pixels vertical
    jb		paintvertl2

    pop		ax
    inc		al
    cmp		al, totalp2lasers
    jb		paintlasersp2

    ret
drawlasersp2 endp

; ############################# FIRE PLAYER LASER BEAM #############################

firelaserplayer1 proc near
    push	ax
    push	bx
    push	cx
    push	dx

    mov		al, totalp1lasers

    lea		bx, lasersp1_x
    add		bl, al
    mov		cx, p1xpos
    mov		[bx], cx

    lea		bx, lasersp1_y
    add		bl, al
    mov		dx, p1ypos
    sub		dx, 5
    mov		[bx], dx

    inc		totalp1lasers

    pop		dx
    pop		cx
    pop		bx
    pop		ax
    ret
firelaserplayer1 endp

firelaserplayer2 proc near
    push	ax
    push	bx
    push	cx
    push	dx

    mov		al, totalp2lasers

    lea		bx, lasersp2_x
    add		bl, al
    mov		cx, p2xpos
    mov		[bx], cx

    lea		bx, lasersp2_y
    add		bl, al
    mov		dx, p2ypos
    sub		dx, 5
    mov		[bx], dx

    inc		totalp2lasers

    pop		dx
    pop		cx
    pop		bx
    pop		ax
    ret
firelaserplayer2 endp

; ############################# KINDA RANDOM PROC #############################

random proc near
    mov		ax, seed						; Move the seed value into AX
    mov		dx, 8405h						; Move 8405H into DX
    mul		dx								; Put 8405H x Seed into DX:AX
    add		ax, 13							; Increment AX
    mov		seed, ax						; We have a new seed
    ret
random endp

; ############################# DELAY PROC #############################

delay proc near
    push	ax
    push	bx
    push	cx
    push	dx

    mov		ah, 00
    int		1Ah
    mov		bx, dx

delaytag:
    int		1Ah
    sub		dx, bx
    cmp		dl, 1							; delay time
    jl		delaytag

    pop		dx
    pop		cx
    pop		bx
    pop		ax
    ret
delay endp

savescreen proc near
    push	ax
    push	cx
    push	dx

    mov		ah, 3ch
    xor		cx, cx
    lea		dx, outputfile
    int		21h

    mov		outhandle, ax
    jc		fileerror

    mov		ah, 3eh
    mov		bx, outhandle
    int		21h
    jmp		endsavescreen
fileerror:
    mov		ah, 4ch
    int		21h

endsavescreen:
    pop		dx
    pop		cx
    pop		ax
    ret
savescreen endp

; prints to sreen the numbers in ax
dispx proc near
    push	dx								; save reg values
    push	cx
    push	bx

    xor		cx, cx
    mov		bx, 10
dispx1:
    xor		dx, dx
    div		bx
    push	dx								; save remainder
    inc		cx								; count remainder
    or		ax, ax							; test quotient
    jnz		dispx1							; if not zero
dispx2:
    pop		dx								; display numbers
    mov		ah, 6
    add		dl, 30h							; convert to ascii
    int		21h
    loop	dispx2							; repeat

    pop		bx								; restore previous reg values
    pop		cx
    pop		dx
    ret
dispx endp

timeproc proc near

    xor     ax, ax
    xor     bx, bx
    xor     cx, cx
    xor     dx, dx

    cmp     totaltime, 0
    jne     gettime
    mov     isgamealive, 0
gettime:
    MOV     AH, 2ch                    ; get system time service
    INT     21h                                       
    CMP     dh, time
    JE      nextsecond                ; no second passed
    
    dec     totaltime                 ; decrements total game secs

    MOV     AH,02H
    XOR     BX,BX
    MOV     dh,17
    MOV     dl,36
    INT     10H

    mov     ax, totaltime ; totaltime = 10
    call    dispx

nextsecond:
    MOV     time, dh                         ;Coloca o segundo atual , na variável timeee
    RET
timeproc endp

code ends
end
