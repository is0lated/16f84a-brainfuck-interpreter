;######################################
;#         brain fuck for pic
;######################################
;# Author       : Matthew King
;# Date Started : 19/11/2010
;######################################
;# Current version: 0.1b
;# Date           : 21/11/2010
;# Notes:
;# Not really sure how this is going to
;# go, but I'll give it a shot.
;# Done:
;# o Software stack
;# o Reading program from store (internal EEPROM for now)
;# o Perform actions in code
;# o Input/Output (Port B for now)
;######################################
;# Fixes:
;#
;# 0.1b:
;# o Main loop is now a loop
;# o Fixed movlw movwf typo in loope
;# o Fixed error treating '.' as input and ',' as output
;#
;# Note:
;# All future fixes are in git history, this is kept purely
;# for historical purposes.

; RAM layout
;
; 0x00:
;	Data at pointer
; 0x02:
;	Position for pointer
; 0x0c:
;	position for stack
; 0x0d - 0x13:
;	Software stack, 
; 0x14 - 0x4E:
;	Positions for working
; 0x4F:
;	Loop seek control (Find the matching ']' to the current '[')

; Brainfuck symbol -> interpreter symbol
;        7      0
; '>' -> 00000000
; '<' -> 00000001
; '+' -> 00000010
; '-' -> 00000011
; '.' -> 00000100
; ',' -> 00000101
; '[' -> 00000110
; ']' -> 00000111
;

;Time for some includes
	list	p=pic16f84a
	include	p16f84a.inc

;Go here for start
start:
	bsf 	03h, 5	;Change to register 1
	movlw	0x00
	movwf	86h		;Set Port B to output
	movlw	0x01
	movwf	85h		;Set A0 to input
	bcf		83h, 5	;Change back to register 0
	clrf	09h		;Start at the beginning of the EEPROM
	clrf	05h		;Clear any errors displayed from last time on Port A
	clrf	06h		;Clear the input/outputs on Port B

;Now to clear the RAM
clrram:
	movlw	0x0C
	movwf	04h		;Set the hardware pointer to the beginning of user RAM
clrramloop:
	clrf	00h		;Clear the data at the pointer
	incf	04h, 1	;Move to the next position in RAM
	movlw	0x50
	subwf	04h, 0	;Subtract the last position in RAM from the current position
	btfss	03h, 2	;A zero resulted if they are the same
	goto	clrramloop ;If it wasn't zero, keep clearing RAM
					;Else continue with the program

	movlw	0x14
	movwf	04h		;Set the pointer to the beginning of the working space

;Now for the bit where everything happens
main:
	call 	getcmd	;Get the command from the EEPROM
	call	intcmd	;Interperate the command
	goto	main	;Continue running

getcmd:
	movlw	40h		;Move out of bounds position into wreg
	subwf	09h, 0	;Subtract the first out of bounds position from the position we're about to read
	btfsc	03h, 2	;Check to see if they matched
	goto	errend	;If they did, there's an error
	
	;Otherwise, continue
	bsf		03h, 5	;We move to bank 1
	bsf		88h, 0	;Read the command at the EEPROM address
	bcf		03h, 5	;Move back to bank 0
	incf	09h, 1	;Move the address for next time
	return			;Go back to the main loop


intcmd:
	btfss	08h, 2	;Seperate I/0 and loop control from movement and modification
	goto	imovmod	;Go to the movmod call command. This is done to allow room for the return
	
	call	ioloop	;If the bit was set it's either I/O or loop control
	return			;Return, we're done
imovmod:
	call	movmod	;If the bit was clear it's either movement or modification
	return			;Return, we're done


ioloop:
	btfss	08h, 1	;Seperate I/O from loop control
	goto	iio		;It was clear, it's I/O
	call	loop	;It was set, it's something to do with loop control
	return			;We're done
iio:
	call	io		;Let's do the I/O shuffle
	return			;We're done

loop:	
	btfss	08h, 0	;Seperate beginning of loop from end
	goto	iloopb	;It's clear, it's the start
	call	loope	;It was set, it's the end of a loop
	return			;well, that's that done
iloopb:
	call	loopb	;It was the start of a loop
	return			;All done


loope:
	movf	0Ch, 1	;Check to make sure there's a beginning for this end
	btfsc	03h, 2	;Check to see if the stack counter is empty
	goto	errend	;If there's not, go to the error

	movf	04h, 0	;Move the current position of the pointer into wreg
	movwf	4Fh		;Place it in the loop control byte temporarily

	movf	0Ch, 0	;Move the stack counter to wreg
	addlw	0x0C	;Add the offset of the stack counter. (counter + offset = postition of stack)
	movwf	04h		;Point the pointer to the stack position

	movf	00h, 0	;Move the position stored in the stack into wreg
	movwf	09h		;Put the address into the next position for the program
	decf	0Ch, 1	;Decrement the stack counter

	movf	4Fh, 0	;Move the temporarily stored pointer position into wreg
	movwf	04h		;Place it back into the pointer address

	return			;AAAAaaaannnnn we're done


loopb:
	movlw	0x07	;Put the final stack count into wreg	
	subwf	0Ch, 0	;Subtract the final stack count from the current count
	btfsc	03h, 2	;Check to see if the counter is already full
	goto	errend	;If the zero bit was set, it was full. Go to the error

	movf	00h, 1	;Check the byte at the pointer
	btfsc	03h, 2	;Check to see if it's empty
	goto	loopbskip	;if it's zero (empty), skip this loop

	movf	04h, 0	;Move the pointer position into wreg
	movwf	4Fh		;Store it temporarily

	incf	0Ch, 1	;Increment the pointer
	movf	0Ch, 0	;Move the pointer offset into wreg
	addlw	0x0C	;Add the offset of the stack counter. (counter + offset = postition of stack)
	movwf	04h		;Put the position of the item into the pointer address

	decf	09h, 0	;Get the current position of this instruction. (Next instruction - 1 = this instruction)
	movwf	00h		;Store the position of the current instruction at the pointer

	movf	4Fh, 0	;Move the temporarily stored pointer position into wreg
	movwf	04h		;Place it back into the pointer address

loopbend:
	return

loopbskip:
	clrf	4Fh		;Clear the loop control byte
	incf	4Fh		;We have 1 open loop to start
lpbskiplp:		;A loop until we find the matching ']'
	call	getcmd	;Get the next command

	movlw	0x06	;Get the byte for a '['
	subwf	08h, 0	;Subtract the byte for '[' from the current command, store result in wreg
	btfsc	03h, 2	;Check to see if they matched
	goto	newloop	;If it matched, there's another loop inside

	movlw	0x07	;Get the byte for a ']'
	subwf	08h, 0	;Subtract the byte for ']' from the current command, store result in wreg
	btfsc	03h, 2	;Check to see if they matched
	goto	closeloop	;If it matched, a loop closed

	goto	lpbskiplp	;If it didn't open or close a loop, keep going

newloop:
	incf	4Fh, 1	;Increment the loop control byte
	goto	lpbskiplp	;Continue the loop

closeloop:
	decfsz	4Fh, 1	;Decrement the loop control byte
	goto	lpbskiplp	;If that doesn't close the first start loop, continue looping
	goto	loopbend	;If it is, exit begin loop.


io:
	btfss	08h, 0	;Check to see if it's input a byte, or output a byte
	goto	ioutput	;If it's clear, we want to output a byte
	call	input	;It it's set, we want to input a byte
	return
ioutput:
	call	output	;Let's output a byte
	return

btoout:	;set Port B to out
	bsf 	03h, 5	;Change to register 1
	movlw	0x00
	movwf	86h		;Set Port B to output
	bcf		83h, 5	;Change back to register 0
	return

btoin:	;set Port B to in
	bsf 	03h, 5	;Change to register 1
	movlw	0xFF
	movwf	86h		;Set Port B to output
	bcf		83h, 5	;Change back to register 0
	return

output:
	call	btoout	;Set Port B to outputs
	movf	00h, 0	;Move the data at the pointer to wreg
	movwf	06h		;Set PortB
	call	btnloop	;Wait for the button to be pressed and released
	clrf	06h		;clear PortB
	return


btnloop:
	movlw	0xFF
	movwf	4Fh		;Get ready for the delay loop

waitinit:
	btfss	05h, 0	;Check to see if the button has been pressed
	goto	waitinit	;If it isn't being held down, go back and wait

btndnloop:
	decfsz	4Fh, 1	;Count down the delay
	goto	btndnloop	;If it's not zero, keep looping

	btfsc	05h, 0	;Check to see if the button is still held down
	goto	waitup	;If it's still down, now wait for it to come up

	movwf	4Fh		;If it's not still held down, reset the delay (This is not needed)
	goto	waitinit	;Go back and wait for it to be held down again

waitup:
	btfsc	05h, 0	;Check to see if the button has been released
	goto	waitup

btnuploop:
	incfsz	4Fh, 1	;Count up the delay
	goto	btnuploop	;If it's not zero, keep looping

	btfss	05h, 0	;Check to see if the button is still released
	goto	btnexit	;If it's still released, go to the exit of the button loop

	clrf	4Fh		;If it hasn't been released, reset the delay (This is not needed)
	goto	waitup	;Go back and wait for it to be released

btnexit:
	return			;The button loop is done, let's go back


input:
	call	btoin	;Set Port B to inputs
	call	btnloop	;Wait for the button to be pressed and released
	movf	06h, 0	;Move Port B into wreg
	movwf	00h		;Place the input at the pointer
	return


movmod:
	btfss	08h, 1	;Check to see if it's going to be movement or modification
	goto	imove	;If it's not set, it's movement
	call	modify	;If it's set, it's modification. Go do that
	return			;Go back
imove:
	call	move	;let's go move the pointer
	return			;Go back


modify:
	btfss	08h, 0	;Check to see if it's up or down
	goto	imodup	;If the bit's clear, it's upwards
	call	moddown	;Otherwise, go move it down
	return			;Done
imodup:
	call	modup	;Go move it up
	return			;Done


moddown:
	decf	00h, 1	;Lower the byte at the pointer by one
	return

modup:
	incf	00h, 1	;Raise the byte at the pointer by one
	return

move:
	btfss	08h, 0	;Check to see if we're moving forward or backward
	goto	iforward	;If it's clear, we're moving forward
	call	backward	;If it's set, we're moving backward
	return				;Go back
iforward:
	call	forward		;Move us forward
	return				;Go back

backward:
	decf	04h, 1		;Move the pointer backwards one place

	movlw	0x13		;Put the first position that is too low into wreg
	subwf	04h, 0		;Check to see if they're the same
	btfsc	03h, 2		;Check to see if the subtraction resulted in a 0
	goto	errend		;If it did, then there's a problem

	return				;Otherwise, return

forward:
	incf	04h, 1		;Move the pointer forwards one place

	movlw	0x4F		;Put the first position that is too high into wreg
	subwf	04h, 0		;Check to see if they're the same
	btfsc	03h, 2		;Check to see if the subtraction resulted in a 0
	goto	errend		;If it did, then there's a problem

	return				;Otherwise, return

errend:
	movlw	0x1E
	movwf	05h			;Set all of Port A minus the buttons to high

errloop:
	goto	errloop		;Infinite loop time

	end					;for picky compilers
