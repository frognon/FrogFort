bits 16
org 0x0000

%macro NEXT 0
    nop
    nop
    lodsw
    xchg bx,ax
    mov ax, [bx]
    xchg bx,ax
    jmp bx
    int 0x3
    int 0x3
%endmacro

%macro PUSHRSP 1
    lea bp, [bp-2]
    mov [bp], %1
%endmacro

%macro POPRSP 1
    mov %1, [bp]
    lea bp, [bp+2]
%endmacro


%assign WORDLIST_LINK 0

%macro DEFWORD 4 ; name, namelen, flags, label
dcw_%4:
    dw WORDLIST_LINK
    %define WORDLIST_LINK dcw_%4
    db %2+%3
    align 2
    db %1
%4:
    dw DOCOL
%endmacro

%macro DEFCODE 4 ; name, namelen, flags, label
dcn_%4:
    dw WORDLIST_LINK
    %define WORDLIST_LINK dcn_%4
    db %2+%3
    align 2
    db %1
%4:
    dw dcn_code_%4
dcn_code_%4:
%endmacro

%macro DEFVAR 5
    DEFCODE %1, %2, %3, %4
    push dfv_%4
    NEXT
    align 2
dfv_%4:
    dw %5
%endmacro


jmp _init

align 2, db 0

retstack_start: dw 0
datastack_start: dw 0
frogfort_start: dw 0


hexchars: db '0123456789abcdef'
msg_perror: db 'errcode:0x', 0
msg_status: db 'status:0x', 0

msg_frogfort_loaded: db 'FrogFort init complete.', 0xa,0xd,0x0



align 2, db 0
retstack_bottom:
    ; 255 bytes return stack, 128 words should be enough for everyone
    times 0xff db 0x0
    ; times 0x40 db 'CALL'
retstack_top:

align 2, db 0
datastack_bottom:
    ; 2 KiB datastack, 1024 words should be enough for everyone
    times 0x800 db 0x0
    ;times 0x200 db 'DATA'
datastack_top:

%define INPUT_BUFFER_LENGTH 0xff
input_buffer: times INPUT_BUFFER_LENGTH db 0x0


align 2, db 0
DOCOL:
    PUSHRSP si
    add ax, 2
    mov si, ax
    NEXT

align 2, db 0
_init:
    mov ax, cs
    mov ds, ax
    mov es, ax
    mov ss, ax
 
    mov bp, retstack_top
    mov sp, datastack_top
    mov word [retstack_start], bp
    mov word [datastack_start], sp
    mov word [frogfort_start], frogfort

    mov si, msg_frogfort_loaded
    call _printstr
    jmp _xxx
    debug_str1: db 'DONE is at ',0
    debug_str2: db 0xa,0xd,'input_buffer is at ',0
    debug_str3: db 0xa,0xd,'frogfort is at ',0
_xxx:
    mov si, debug_str1
    call _printstr
    mov ax, DONE
    call _print_ax_as_hex
    mov si, debug_str2
    call _printstr
    mov ax, input_buffer
    call _print_ax_as_hex
    mov si, debug_str3
    call _printstr
    mov ax, frogfort
    call _print_ax_as_hex
    call _printnewline

    mov si, frogfort
    NEXT


frogfort:
    dw LATEST, POPANDPRINTSTACK, PNLN, LIT, 0x6884, POPANDPRINTSTACK, PNLN, DONE
    ;dw READSTRING, PNLN, LIT, input_buffer, PRINTSTRING, PNLN, DONE
    ;dw PUSH0, ADD1, ADD3, PNLN, PHEX, PNLN, PNLN, READSTRING, DONE

_printstr:
    mov ax,0x0e00
__printstr_1:
    lodsb
    cmp al, 0
    je __printstr_end
    int 0x10
    jmp __printstr_1
__printstr_end:
    ret

_print_al_as_hex:
    push si
    mov si, hexchars
    mov dx, ax
    mov bx, ax
    mov cl, 4
    mov ah, 0xe
    and bx,0xf
    mov al, [bx+si]
    push ax
    mov bx, dx
    shr bx, cl
    and bx, 0xf
    mov al, [bx+si]
    int 0x10
    pop ax
    int 0x10
    pop si
    ret


_print_ax_as_hex:
    push ax
    mov cl, 8
    shr ax, cl
    and ax, 0xff
    call _print_al_as_hex
    pop ax
    and ax, 0xff
    call _print_al_as_hex
    ret

_printnewline:
    push ax
    mov ax,0x0e0a
    int 0x10
    mov al, 0x0d
    int 0x10
    pop ax
    ret

_pstatus:
    push ax,
    mov si, msg_status
    call _printstr
    pop ax
    call _print_ax_as_hex
    call _printnewline
    ret

_perror:
    push ax ; error code in ax, save, print error message prefix
    mov si, msg_perror
    call _printstr
    pop ax ; restore error code
    call _print_ax_as_hex
    call _printnewline
    ret


DEFVAR 'LATEST', 6, 0, LATEST, DONE

align 2, db 0
DEFCODE 'PUSH0', 5, 0, PUSH0
    xor ax,ax
    push ax
NEXT

DEFCODE 'ADD1', 4, 0, ADD1
    pop ax
    inc ax
    push ax
NEXT

DEFCODE 'EXIT', 4, 0, EXIT
    POPRSP si
NEXT

DEFCODE 'PHEX', 4, 0, PHEX
    pop ax
    call _print_ax_as_hex    
NEXT

DEFCODE 'PNLN', 4, 0, PNLN
    mov ax,0x0e0a
    int 0x10
    mov al, 0x0d
    int 0x10
NEXT


DEFCODE '.', 1, 0, POPANDPRINTSTACK
    pop ax
    call _print_ax_as_hex
NEXT

DEFCODE 'DROP', 4, 0, DROP
    pop ax
NEXT

DEFCODE 'DUP', 3, 0, DUP
    mov bx, sp
    mov word ax, [bx]
    push ax
NEXT



DEFWORD 'ADD3', 4, 0, ADD3
    dw ADD1
    dw ADD1
    dw ADD1
    dw EXIT
NEXT

DEFCODE 'PRINTSTRING', 11, 0, PRINTSTRING
    pop ax
    push si
    mov si, ax
    call _printstr
    pop si
NEXT

DEFCODE 'LIT', 3, 0, LIT
    lodsw
    push ax
NEXT


DEFCODE 'READSTRING', 10, 0, READSTRING
    pusha
    xor cx,cx
    mov di, input_buffer
_readstring_start:
    xor ax,ax
    int 0x16
    cmp al, 0xd  ; newline
    je _readstring_done
    ; echo whatever was input
    mov ah,0xe
    int 0x10

    ; was it backspace?
    cmp al, 0x8 ; backspace
    jne _readstring_inputok

    ; handle backspace
    ; jump back
    cmp cx, 0
    je _readstring_start
    dec cx
    dec di
    jmp _readstring_start

    ; wasn't backspace, store content
_readstring_inputok:
    stosb
    inc cx

    ; ensure input buffer does not overflow
    cmp cx, INPUT_BUFFER_LENGTH-1
    jge _readstring_done

    jmp _readstring_start
_readstring_done:
    ; zero terminate string
    xor ax,ax
    stosb
    popa
NEXT

; TODO
; defcode PUTSTR ( addr len -- )
; defcode CMPSTR ( addra addrb len -- equal?)

; ( pName len -- maybe_addr?)
; scan through dictionary looking or word matching pName
DEFCODE 'FIND', 4, 0, DONE
    pop cx
    pop ax
NEXT

DEFCODE 'DONE', 4, 0, DONE
    jmp _done_loop
_done_text: db 'FrogFort stopped.',0xa,0xd,0x0
_done_loop:
    push si
    mov si, _done_text
    call _printstr
    pop si
    xor ax, ax
    int 0x16
    jmp _done_loop
NEXT ; note that this will not get called


times 65535-($-$$) db 0xcc
