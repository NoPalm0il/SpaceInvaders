setupscreen macro								; setup video mode
	xor     ah, ah								; graph mode ah:=0
	mov     al, 04h								; 320x200 color mode
	int     10h
; hide cursor
	mov     ch, 32								; sets the cursor size
	mov     ah, 1								; cursor size service
	int     10h
; palete de cores
	mov     ax, 0bh								; color palette for display mode
	mov     bh, 01								; palette id := 1, palette for 320 x 200
	xor     bl, bl								; color or palette(0) to be used with color id
	int     10h
endm

normalscreen macro								; sets the normal video mode
	xor     ah, ah
	mov     al, 02h								; normal video mode (text)
	int     10h
endm

stack segment para stack
	db 64 dup ('mystack')
stack ends

data segment para 'data'
	;data
	p1xpos		dw	50							; p1 x pos
	p1ypos		dw	150							; p1 y pos
	p2xpos		dw	150							; p2 x pos
	p2ypos		dw	150							; p2 y pos
data ends

code segment para 'code'

main proc far
	assume cs:code,ds:data,es:data,ss:stack
	push 	ds
	sub 	ax,ax
	push 	ax
	mov 	ax,data
	mov 	ds,ax
	mov 	es,ax

	setupscreen									; macro to setup the video mode

	call gameloop								; main game loop

	normalscreen								; macro to setup normal text mode

	ret

main endp

gameloop proc near

	call paintplayer1
	call paintplayer2
	call paintenemies

	mov		ah,01h
mainloop:
	int		16h									; ZF = 0 if char avaiable

	call paintenemies

	jz		mainloop							; is a char avaiable in the keyboard buffer?
	mov		ah,00h								; clears the buffer and flag
	int		16h

	cmp		al,"w"		
	je		p1up								; player 1 up
	cmp		al,"a"		
	je		p1lf								; player 1 left
	cmp		al,"s"		
	je		p1dw								; player 1 down
	cmp		al,"d"		
	je		p1rt								; player 1 right

	cmp		al,"i"		
	je		p2up								; player 2 up
	cmp		al,"j"		
	je		p2lf								; player 2 left
	cmp		al,"k"		
	je		p2dw								; player 2 down
	cmp		al,"l"		
	je		p2rt								; player 2 right

	cmp		al,"q"
	je		exit

	jmp		mainloop

p1up:
	dec		p1ypos
	jmp		paintp1
p1lf:
	dec		p1xpos
	jmp		paintp1
p1dw:
	inc		p1ypos
	jmp		paintp1
p1rt:
	inc		p1xpos
	jmp		paintp1
paintp1:
	call 	paintplayer1

	jmp 	mainloop
p2up:
p2lf:
p2dw:
p2rt:
paintp2:
	call 	paintplayer2

	jmp		mainloop
exit:
	ret
gameloop endp


paintplayer1 proc near
	push	ax
	push	bx
	push	cx
	push	dx

	mov		ah, 0ch
	mov		al, 01h
	mov		cx, p1xpos							; horizontal pos
	mov		dx, p1ypos							; vert pos

	call 	paintplayerbox

	pop		dx
	pop		cx
	pop		bx
	pop		ax

	ret
paintplayer1 endp

paintplayer2 proc near
	push	ax
	push	bx
	push	cx
	push	dx

	mov		ah, 0ch
	mov		al, 04h
	mov		cx, p2xpos							; horizontal pos
	mov		dx, p2ypos							; vert pos

	call 	paintplayerbox

	pop		dx
	pop		cx
	pop		bx
	pop		ax

	ret
paintplayer2 endp

paintenemies proc near
	push	ax
	push	cx
	push	dx
	
	mov		ah, 0ch
	mov		al, 1
	mov		cx, 30
	mov		dx, 30
paintenemypx:
	int		10h
	inc		al
	cmp		al, 7
	jbe		paintenemypx

	pop		dx
	pop		cx
	pop		ax
	
	ret
paintenemies endp


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

code ends
end