setupscreen macro													; setup video mode
	xor		ah, ah														; graph mode ah:=0
	mov		al, 04h														; 320x200 color mode
	int		10h
; hide cursor
	mov		ch, 32														; sets the cursor size
	mov		ah, 1															; cursor size service
	int		10h
; palete de cores
	mov		ax, 0dh														; color palette for display mode
	xor		bl, bl														; color or palette(0) to be used with color id
	int		10h
endm

normalscreen macro												; sets the normal video mode
	xor		ah, ah
	mov		al, 03h														; normal video mode (text)
	int		10h
endm

paintscoreline macro
	mov 	cx, 240
	mov		dx, 200
paintlineloop:
	mov		al, 01h
	mov		ah, 0ch
	int		10h
	dec		dx
	cmp		dx, 0
	jnz		paintlineloop
endm

setupenemiescords macro
	mov		cx, 4
	lea		bx, enemyshipxpos
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
	loop loopcoords
endm

stack segment para stack
	db 64 dup ('mystack')
stack ends

data segment para 'data'
	seed 						dw 	9371h
	p1xpos					dw	50									; p1 x pos
	p1ypos					dw	150									; p1 y pos
	p2xpos					dw	150									; p2 x pos
	p2ypos					dw	150									; p2 y pos

	enemyshipxpos		dw	0h,0h,0h,0h
	enemyshipypos		dw	0h,0h,0h,0h

	eneypos					dw	30

	outputfile 			db "printscreen.ppm",0
	outhandle 			dw ?

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

	setupscreen															; macro to setup the video mode
	paintscoreline
	setupenemiescords												; macro for setting up enemies coords
	call	gameloop													; main game loop

	normalscreen														; macro to setup normal text mode

	ret

main endp

gameloop proc near

	mov		al, 01h														; player color
	call	paintplayer1
	mov		al, 02h														; player color
	call	paintplayer2
	call 	paintenemies

mainloop:												; ################## MAIN LOOP ##################

	call 	delay
	call	paintenemies
	call 	readchar

	cmp		al,"w"
	je		p1up															; player 1 up
	cmp		al,"a"
	je		p1lf															; player 1 left
	cmp		al,"s"
	je		p1dw															; player 1 down
	cmp		al,"d"
	je		p1rt															; player 1 right
	cmp		al,"x"
	je		shootp1

	cmp		al,"u"		
	je		p2up															; player 2 up
	cmp		al,"h"		
	je		p2lf															; player 2 left
	cmp		al,"j"		
	je		p2dw															; player 2 down
	cmp		al,"k"		
	je		p2rt															; player 2 right
	cmp		al,"n"
	je		shootp2

	cmp		al,"g"
	je		saveimg
	cmp		al,"q"
	je		exit

	jmp		mainloop
p1up:
	call	p1upproc
	jmp		paintp1
p1dw:
	call p1dwproc
	jmp		paintp1
p1lf:
	call p1lfproc
	jmp		paintp1
p1rt:
	call p1rtproc
	jmp		paintp1
paintp1:
	mov		al, 01h														; player color
	call	paintplayer1
	jmp		mainloop
shootp1:
	call	firebeamplayer1
	jmp		mainloop
saveimg:
	call 	savescreen
	jmp		mainloop
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
	mov		al, 02h														; player color
	call 	paintplayer2
	jmp 	mainloop
shootp2:
	call	firebeamplayer2
	jmp		mainloop

	ret
gameloop endp

p1upproc proc near
	call	removeplayer1color
	dec		p1ypos
	dec		p1ypos
	ret
p1upproc endp
p1dwproc proc near
	call	removeplayer1color
	inc		p1ypos
	inc		p1ypos
	ret
p1dwproc endp
p1lfproc proc near
	call	removeplayer1color
	dec		p1xpos
	dec		p1xpos
	ret
p1lfproc endp
p1rtproc proc near
	call	removeplayer1color
	inc		p1xpos
	inc		p1xpos
	ret
p1rtproc endp

p2upproc proc near
	call	removeplayer2color
	dec		p2ypos
	dec		p2ypos
	ret
p2upproc endp
p2dwproc proc near
	call	removeplayer2color
	inc		p2ypos
	inc		p2ypos
	ret
p2dwproc endp
p2lfproc proc near
	call	removeplayer2color
	dec		p2xpos
	dec		p2xpos
	ret
p2lfproc endp
p2rtproc proc near
	call	removeplayer2color
	inc		p2xpos
	inc		p2xpos
	ret
p2rtproc endp

; ############################# READ CHAR #############################

readchar proc
	mov		ah, 01h
	int		16h
	jnz		keybdpressed
	xor		dl, dl
	ret
keybdpressed:
	xor		ah, ah
	int		16h																; extract the keystroke from the buffer, clears zf and buffer
	ret
readchar endp    

; ############################# REMOVE PLAYER 1 PROC #############################

removeplayer1color proc near
	push	ax
	mov		al, 00														; paint black
	call 	paintplayer1
	pop		ax
	ret
removeplayer1color endp

; ############################# REMOVE PLAYER 2 PROC #############################

removeplayer2color proc near
	push	ax
	mov		al, 00														; paint black
	call 	paintplayer2
	pop		ax
	ret
removeplayer2color endp

; ############################# PAINT PLAYER 1 PROC #############################

paintplayer1 proc near
	push	bx
	push	cx
	push	dx

	mov		ah, 0ch
	mov		cx, p1xpos												; horizontal pos
	mov		dx, p1ypos												; vert pos

	call 	paintplayerbox

	pop		dx
	pop		cx
	pop		bx

	ret
paintplayer1 endp

; ############################# PAINT PLAYER 2 PROC #############################

paintplayer2 proc near
	push	bx
	push	cx
	push	dx

	mov		ah, 0ch
	mov		cx, p2xpos												; horizontal pos
	mov		dx, p2ypos												; vert pos

	call 	paintplayerbox

	pop		dx
	pop		cx
	pop		bx

	ret
paintplayer2 endp

; ############################# PAINT ENEMIES #############################

paintenemies proc near
	push	ax
	push	bx
	push	cx
	push	dx

	mov		cx, 4
	lea		bx, enemyshipxpos
	
paintblackenemyloop:											; ### PAINTS BLACK ###
	push	cx
	mov		ah, 0ch
	xor		al, al
	mov		cx, [bx]
	xor		ch, ch
	mov		dx, eneypos
	call 	paintenemybox											; paints black box

	inc		bx
	pop		cx
	loop paintblackenemyloop

	inc		eneypos

	lea		bx, enemyshipxpos
	mov		cx, 4
paintenemyloop:														; ### PAINTS COLOR ###
	push	cx
	mov		ah, 0ch
	mov		al, 100b													; red color
	mov		cx, [bx]
	xor		ch, ch
	mov		dx, eneypos

	call 	paintenemybox
	inc		bx
	pop		cx
	loop	paintenemyloop

	pop		dx
	pop		cx
	pop		bx
	pop		ax
	
	ret
paintenemies endp

; ############################# PAINT PLAYER BOX #############################

paintplayerbox proc near
	push	bx
	sub		cx, 4															; top left corner
	sub		dx, 3															; top left corner
	xor		bx, bx														; 9 pixels right

paintyplayer:
	cmp		bl, 7
	je		endplayerloop
	xor		bh, bh
paintxplayer:
	cmp		bh, 9
	je continueplayer
	push	bx
	xor		bh, bh
	int		10h
	pop		bx
	inc		cx
	inc		bh
	jmp		paintxplayer
continueplayer:
	inc		bl
	inc		dx
	sub		cx, 9
	jmp		paintyplayer
endplayerloop:
	pop		bx
	ret
paintplayerbox endp

; ############################# PAINT ENEMY BOX #############################
; receives colors and positions
paintenemybox proc near
	push	bx
	push	cx
	push	dx

	sub		cx, 3															; top left corner (cx = x)
	sub		dx, 3															; top left corner (dx = y)
	xor		bx, bx

paintyenemy:
	cmp		bl, 7
	je		endenemyloop
	xor		bh, bh
paintxenemy:
	cmp		bh, 7
	je continueenemy
	push	bx
	xor		bh, bh
	int		10h
	pop		bx
	inc		cx
	inc		bh
	jmp		paintxenemy
continueenemy:
	inc		bl
	inc		dx
	sub		cx, 7
	jmp		paintyenemy

endenemyloop:
	pop		dx
	pop		cx
	pop		bx
	ret
paintenemybox endp

; ############################# FIRE PLAYER LASER BEAM #############################

firebeamplayer1 proc near
	push	cx
	push	dx

	mov		ah, 0ch
	mov		al, 01h
	xor		bx, bx
	mov		cx, p1xpos
	mov		dx,	p1ypos
	sub		dx, 4
loopbeamp1:
	int		10h
	dec		dx
	cmp		dx, 0
	jnz		loopbeamp1

	call	delay

	xor		al, al														; pixel color
	mov		dx,	p1ypos
	sub		dx, 4
loopbeamp1del:
	int		10h
	dec		dx
	cmp		dx, 0
	jnz		loopbeamp1del

	pop		dx
	pop		cx
	ret
firebeamplayer1 endp

firebeamplayer2 proc near
	push	cx
	push	dx

	mov		ah, 0ch
	mov		al, 02h
	xor		bx, bx
	mov		cx, p2xpos
	mov		dx,	p2ypos
	sub		dx, 4
loopbeamp2:
	int		10h
	dec		dx
	cmp		dx, 0
	jnz		loopbeamp2

	call	delay

	xor		al, al														; pixel color
	mov		dx,	p2ypos
	sub		dx, 4
loopbeamp2del:
	int		10h
	dec		dx
	cmp		dx, 0
	jnz		loopbeamp2del

	pop		dx
	pop		cx
	ret
firebeamplayer2 endp

; ############################# KINDA RANDOM PROC #############################

random proc near
	mov		ax, seed													; Move the seed value into AX
	mov		dx, 8405h													; Move 8405H into DX
	mul		dx																; Put 8405H x Seed into DX:AX
	add		ax, 13														; Increment AX
	mov		seed, ax													; We have a new seed
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
	cmp		dl, 1															; delay time                                                      
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

code ends
end