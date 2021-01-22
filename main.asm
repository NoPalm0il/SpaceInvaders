setupscreen macro							; setup video mode
	xor		ah, ah								; graph mode ah:=0
	mov		al, 04h								; 320x200 color mode
	int		10h
; hide cursor
	mov		ch, 32								; sets the cursor size
	mov		ah, 1									; cursor size service
	int		10h
; palete de cores
	mov		ax, 0bh								; color palette for display mode
	mov		bh, 01								; palette id := 1, palette for 320 x 200
	xor		bl, bl								; color or palette(0) to be used with color id
	int		10h
endm

normalscreen macro						; sets the normal video mode
	xor		ah, ah
	mov		al, 03h								; normal video mode (text)
	int		10h
endm

paintscoreline macro
	mov 	cx, 200
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
	seed 			dw 	9371h
	p1xpos		dw	50							; p1 x pos
	p1ypos		dw	150							; p1 y pos
	p2xpos		dw	150							; p2 x pos
	p2ypos		dw	150							; p2 y pos

	enemyshipxpos		dw	0h,0h,0h,0h
	enemyshipypos		dw	0h,0h,0h,0h

	enexpos		dw	40
	eneypos		dw	30

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

	setupscreen										; macro to setup the video mode
	paintscoreline
	setupenemiescords							; macro for setting up enemies coords
	call	gameloop								; main game loop

	normalscreen									; macro to setup normal text mode

	ret

main endp

gameloop proc near

	mov		al, 01h									; player color
	call	paintplayer1
	mov		al, 02h									; player color
	call	paintplayer2
	call 	paintenemies

mainloop:												; ################## MAIN LOOP ##################

	call 	delay
	call	paintenemies
	call 	readchar

	cmp		al,"w"
	je		p1up										; player 1 up
	cmp		al,"a"
	je		p1lf										; player 1 left
	cmp		al,"s"
	je		p1dw										; player 1 down
	cmp		al,"d"
	je		p1rt										; player 1 right

	cmp		al,"i"		
	je		p2up										; player 2 up
	cmp		al,"j"		
	je		p2lf										; player 2 left
	cmp		al,"k"		
	je		p2dw										; player 2 down
	cmp		al,"l"		
	je		p2rt										; player 2 right

	cmp		al,"q"
	je		exit

	jmp		mainloop

p1up:
	call	removeplayer1color
	dec		p1ypos
	dec		p1ypos
	jmp		paintp1
p1lf:
	call	removeplayer1color
	dec		p1xpos
	dec		p1xpos
	jmp		paintp1
p1dw:
	call	removeplayer1color
	inc		p1ypos
	inc		p1ypos
	jmp		paintp1
p1rt:
	call	removeplayer1color
	inc		p1xpos
	inc		p1xpos
	jmp		paintp1
paintp1:
	mov		al, 01h									; player color
	call	paintplayer1
	jmp		mainloop
exit:
	ret
p2up:
	call	removeplayer2color
	dec		p2ypos
	dec		p2ypos
	jmp		paintp2
p2lf:
	call	removeplayer2color
	dec		p2xpos
	dec		p2xpos
	jmp		paintp2
p2dw:
	call	removeplayer2color
	inc		p2ypos
	inc		p2ypos
	jmp		paintp2
p2rt:
	call	removeplayer2color
	inc		p2xpos
	inc		p2xpos
	jmp		paintp2
paintp2:
	mov		al, 02h									; player color
	call 	paintplayer2
	jmp 	mainloop

	ret
gameloop endp

; ############################# READ CHAR #############################

readchar proc
    mov 	ah, 01H
    int 	16H
    jnz 	keybdpressed
    xor 	dl, dl
    ret
keybdpressed:
    ;extract the keystroke from the buffer
    xor		ah, ah
    int 	16H
    ret
readchar endp    

; ############################# REMOVE PLAYER 1 PROC #############################

removeplayer1color proc near
	push	ax
	mov		al, 00									; paint black
	call 	paintplayer1
	pop		ax
	ret
removeplayer1color endp

; ############################# REMOVE PLAYER 2 PROC #############################

removeplayer2color proc near
	push	ax
	mov		al, 00									; paint black
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
	mov		cx, p1xpos							; horizontal pos
	mov		dx, p1ypos							; vert pos

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
	mov		cx, p2xpos							; horizontal pos
	mov		dx, p2ypos							; vert pos

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
	
paintblackenemyloop:							; ### PAINTS BLACK ###
	push	cx
	mov		ah, 0ch
	xor		al, al
	mov		cx, [bx]
	xor		ch, ch
	mov		dx, eneypos
	call 	paintenemybox							; paints black box

	inc		bx
	pop		cx
	loop paintblackenemyloop

	inc		eneypos

	lea		bx, enemyshipxpos
	mov		cx, 4
paintenemyloop:										; ### PAINTS COLOR ###
	push	cx
	mov		ah, 0ch
	mov		al, 01h
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
	sub		cx, 4								; top left corner
	sub		dx, 3								; top left corner
	mov		bl, 9								; 9 pixels right
painthorztop:
	
	int		10h
	inc		cx
	dec		bl
	cmp		bl, 0
	ja		painthorztop

	mov		bl, 7								; 7 pixels down
paintvertright:
	int		10h
	inc		dx
	dec		bl
	cmp		bl, 0
	ja		paintvertright

	mov		bl, 9
painthorzdown:
	int		10h
	dec		cx
	dec		bl
	cmp		bl, 0
	ja		painthorzdown

	mov		bl, 7								; 7 pixels down
paintvertleft:
	int		10h
	dec		dx
	dec		bl
	cmp		bl, 0
	ja		paintvertleft

	ret
paintplayerbox endp

; ############################# PAINT ENEMY BOX #############################

paintenemybox proc near
	push 	bx
	push	cx
	push	dx
	sub		cx, 4								; top left corner
	sub		dx, 3								; top left corner
	mov		bx, 9								; 9 pixels right
painthorztopenemy:
	
	int		10h
	inc		cx
	dec		bx
	cmp		bx, 0
	ja		painthorztopenemy

	mov		bx, 9								; 9 pixels down
paintvertrightenemy:
	int		10h
	inc		dx
	dec		bx
	cmp		bx, 0
	ja		paintvertrightenemy

	mov		bx, 9
painthorzdownenemy:
	int		10h
	dec		cx
	dec		bx
	cmp		bx, 0
	ja		painthorzdownenemy

	mov		bx, 9								; 9 pixels down
paintvertleftenemy:
	int		10h
	dec		dx
	dec		bx
	cmp		bx, 0
	ja		paintvertleftenemy

	pop		dx
	pop		cx
	pop		bx
	ret
paintenemybox endp

; ############################# KINDA RANDOM PROC #############################

random PROC
	MOV 	AX, Seed ; Move the seed value into AX
	MOV 	DX, 8405H ; Move 8405H into DX
	MUL 	DX ; Put 8405H x Seed into DX:AX
	ADD 	AX, 13 ; Increment AX
	MOV 	Seed, AX ; We have a new seed
	RET
random ENDP

; ############################# DELAY PROC #############################

delay proc 
	mov 	ah, 00
	int 	1Ah
	mov 	bx, dx
    
delaytag:
	int 	1Ah
	sub 	dx, bx
	cmp 	dl, 1								; delay time                                                      
	jl 		delaytag
	ret
delay endp

code ends
end