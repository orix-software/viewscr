; vim: set ft=asm6502-2 ts=8:

.feature string_escapes

;----------------------------------------------------------------------
;			cc65 includes
;----------------------------------------------------------------------
.include "telestrat.inc"
.include "fcntl.inc"

;----------------------------------------------------------------------
;			Orix Kernel includes
;----------------------------------------------------------------------
.include "kernel/src/include/kernel.inc"


;----------------------------------------------------------------------
;			Orix Shell includes
;----------------------------------------------------------------------


;----------------------------------------------------------------------
;			Orix SDK includes
;----------------------------------------------------------------------
.include "SDK.mac"
.include "types.mac"
.include "errors.inc"


;----------------------------------------------------------------------
;				Imports
;----------------------------------------------------------------------
; From sopt
.import calposp
.importzp cbp

; From ermes
.import ermes

;----------------------------------------------------------------------
;				Exports
;----------------------------------------------------------------------
.export _main
;.export _argc
;.export _argv

; Pour ermes
.export crlf1, out1, seter1
.export prfild, prnamd

.export ermtb
.export drive
.exportzp xtrk, psec

;.export dskname

;----------------------------------------------------------------------
;			Librairies
;----------------------------------------------------------------------


;----------------------------------------------------------------------
; Defines / Constants
;----------------------------------------------------------------------

	max_path := KERNEL_MAX_PATH_LENGTH

;----------------------------------------------------------------------
;				Page Zéro
;----------------------------------------------------------------------
.zeropage
	unsigned char xtrk
	unsigned char psec

;----------------------------------------------------------------------
;				Variables
;----------------------------------------------------------------------
.segment "DATA"
	char drive

	unsigned char dskname[max_path]
	unsigned short fp


;----------------------------------------------------------------------
; Variables et buffers
;----------------------------------------------------------------------
.segment "CODE"

;----------------------------------------------------------------------
;			Segments vides
;----------------------------------------------------------------------
.segment "STARTUP"
.segment "INIT"
.segment "ONCE"

;----------------------------------------------------------------------
;				Programme
;----------------------------------------------------------------------
.segment "CODE"

.proc _main

	ldy #<(BUFEDT+.strlen("VIEWSCR"))
	lda #>(BUFEDT+.strlen("VIEWSCR"))
	sty cbp
	sta cbp+1

	; Saute au premier paramètre
	ldy #$00
	jsr calposp

	jsr getfname
	bcs error

  next:
	fopen dskname, O_RDONLY
	sta fp
	stx fp+1

	eor fp+1
	beq errFopen

	jsr cls

	fread SCREEN, $1120, 1, fp
	fclose (fp)

	; Attend l'appui sur une touche
  loop:
	cgetc
	beq loop
	pha

	; Efface l'écran
	jsr cls

	; Fin si CTRL+C
	pla
	cmp #$03
	beq end

	; Fichier suivant
	jsr getfname
	bcc next

  end:
	clc
	rts

  errFopen:
	lda #e13
	sec

  error:
	pha
	jsr	cmnd_version
	pla
	sec
	jmp	ermes
.endproc


;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
.proc cls
	; Efface l'écran

	; Efface la ligne de status
	lda #<SCREEN
	ldy #>SCREEN
	sta RES
	sty RES+1
	;ldy #<(SCREEN+28*40)
	;ldx #>(SCREEN+28*40)
	ldy #<(SCREEN+1*40)
	ldx #>(SCREEN+1*40)
	lda #' '
	BRK_KERNEL XFILLM

	; Efface le reste de l'écran
	cputc $0c

	rts
.endproc


;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
.proc getfname
	; AY : adresse du paramètre suivant
	; cbp:   ''          ''
	;sty dskname
	;sta dskname+1

	ldy #$ff
  loop:
	iny
	lda (cbp),y
	sta dskname,y
	beq endloop
	cmp #$0d
	beq endloop
	cmp #' '
	bne loop

  endloop:
	cpy #00
	beq error_no_filename

	; Termine la chaîne par un nul
;	cmp #$00
;	beq ajuste

	lda #$00
	;sta (cbp),y
	sta dskname,y
	;iny

	; Ajuste cbp
;  ajuste:
;	clc
;	tya
;	adc cbp
;	sta cbp
;	bcc skip
;	inc cbp+1
;
;  skip:
;	clc
	jsr calposp
	rts

  error_no_filename:
	lda #e12
	sec
	rts
.endproc

;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
.proc cmnd_version
	prints	"viewscr version 1.0 - 2022.2\r\n"
	rts
.endproc

;===========================================================================
;		Gestion des erreurs
;===========================================================================
.segment "CODE"

;----------------------------------------------------------------------
;
;----------------------------------------------------------------------
crlf1:
	crlf
	rts

;----------------------------------------------------------------------
;
;----------------------------------------------------------------------
out1:
	cputc
	rts

;----------------------------------------------------------------------
;
;----------------------------------------------------------------------
.proc prfild
	print dskname
	rts
.endproc

;----------------------------------------------------------------------
;
;----------------------------------------------------------------------
.proc prnamd
	print dskname
	rts
.endproc


;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
seter:
seter1:
	rts

;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
svzp:
	rts

;**********************************************************************
; Fin du programme
;**********************************************************************

;----------------------------------------------------------------------
;				DATAS
;----------------------------------------------------------------------
.segment "RODATA"


;----------------------------------------------------------------------
;				TABLES
;----------------------------------------------------------------------
.segment "RODATA"

	ermtb:
                ; code erreur, "message"
                ;
                ; caractère: >= $80: fin du message
                ;           $ff -> Cr/Lf
                ;           $fe -> " error"
                ;           $fd -> " protected"
                ;           autre -> n° erreur (BCD)
                ;        <  $80:
                ;           >=10 -> affiche le caractère
                ;           <10 -> sous message
                ;             1 -> print filespec (drive ':' repertoire '/' fichier
                ;             2 -> print filename
                ;             3 -> print drive ':' track ':' sector
                ;           >=4 -> print drive
;	        .byte     e1,"Mem. full",$ff
;	        .byte     e2,"Disk full",$ff
;	        .byte     e3,"BOF",$ff
;	        .byte     e4,"EOF",$ff
;	        .byte     e10,"Filename",$fe
;	        .byte     e11,"Device",$fe
	        .byte     e12,"Filename missing",$ff
	        .byte     e13,2,"not found",$ff
;	        .byte     e15,"Option",$fe
;	        .byte     e16,"Data",$fe
;	;       .byte     e20,"No more entries",$ff
;	        .byte     e21,1,"open file",$ff
;	;       .byte     e25,1,"delete",$fd
;	;       .byte     e26,1,"write",$fd
;	;       .byte     e27,1,"read",$fd
;	        .byte     e28,1,"permission denied",$ff
;	        .byte     e29,1,"incorrect format",$ff
;	;       .byte     e30,"File not open",$ff
;	;       .byte     e31,"Fd",$fe
;	        .byte     $80,"Write",3,$fe
;	        .byte     $90,"Read",3,$fe
;	        .byte     $b0,"Dr:tr:sc",3,$fe
;	        .byte     $c0,"Drive",4," not ready",$ff
;	        .byte     $d0,"Disk",4,$fd
	        .byte     0,"Error ",$fc

