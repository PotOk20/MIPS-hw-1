dseg segment 'DATA' 

igrica db 'MIPS hw-1','$' ;naslov
naslovX db 3 
naslovY db 38

skor db 'Score:','$' ;merac poena
inicijalniSkor db 0
skorX db 1
skorY db 5

blok dw '%%%%% ','$' ;cigla
blokX db 3
blokY db 1

platforma db '********','$' ;podloga za pomeranje
platformaX db 23
platformaY db 35

loptica db 'o','$'    ;loptica 
lopticaX db 7
lopticaY db 38

gornjaGranica db 5     ;granice terena 
donjaGranica db 25
levaGranica db 0
desnaGranica db 80

dir1 db 1    ; dole = 0, gore = 1
dir2 db 1   ; desno = 1, levo = 0

dseg ends

cseg	segment	'CODE'
		assume cs:cseg, ds:dseg, ss:sseg

main proc
    mov ax, dseg
    mov ds,ax
     
    call welcome    
    call start 
    
    
    mov AH,4CH
    INT 21H
    
  
    start:
        call clearScreen
    
        mov ah, 2   
        mov bh, 0 
        mov dh, 0
        mov dl, 0  
        int 10h
     
        call printScore    
        call printBlocks
        call printBall
        call printPlatform
        call moveBall
        
        read:
            mov ah, 00h  ;Cita ulaz sa tastature
            int 16h
                    
        cmp ah, 4Dh  ;desna strelica 
        je right
    
        cmp ah, 4Bh  ;leva strelica
        je left
    
        right:                       ;ako je desna strelica 
            cmp platformaY, 72       ;poredi maksimal,nu vrednost Y kordinate sa platormom
            je start                 ;ako su iste, vraca se u petlju igrice
            inc platformaY           ;ako nisu, inkrementuje Y poziciju platforme za 1
            jmp start                ;skace nazad na igru
        
        left:
            cmp platformaY, 0        ;isto to samo za levu stranu
            je start
            sub platformaY, 1
            jmp start
        
    ret

    printBall:                        
        mov ah, 2h
        mov dh, lopticaX
        mov dl, lopticaY
        int 10h
        
        mov ah, 9h
        mov dx, offset loptica
        int 21h
    ret
;------------------------------------------------------------------------------------  pomeranje lopte  --------------------------------------------------------------------------------------------------------------------------;
   moveBall:
        mov bh, lopticaX
        mov bl, lopticaY
        
        inicijalizuj:                                              
            mov AH,2H                ;pocetni ugao
            mov DH, lopticaX
            mov DL, lopticaY
            INT 10H
            
		move:                                                     
			cmp dir1, 0          ; da li je smer ka dole? ako jeste skaci na moveDown
			je moveDown
			cmp dir1, 1          ; ako je smer ka gore skoci na moveUp
			je moveUp           
		
		controlLimit:                                               
			cmp lopticaX, 5         ; da li je loptica dosla do gornje granice?
			je bounceDown           ; ako jeste odskoci na dole
			
			cmp lopticaY, 0         ; uporedjuje poziciju loptice sa levom granicom
			je bounceRight          ; ako jeste, odskace udesno
			
			cmp lopticaY, 80        ; uporedjuje poziciju loptice sa desnom granicom
			je bounceLeft           ; odskace ulevo
			
			cmp lopticaX, platformaY    ;uporedjuje poziciju loptice sa platformom
			je bounceUp  	            ;odskace na gore
			
	
		bounceRight:                                                               
			mov dir2, 1          ; azuriraj smer u desno
			cmp dir1, 1          ; vidi dal loptica ide gore 
			je bounceRightUp     ; ako je isla na gore, odbice se gore desno
			jne bounceRightDown  ; ako je isla na dole, odbice se dole levo
			
			bounceRightUp:
				mov dir1, 1     ; azuriraj smer na gore-desno
				 
			bounceRightDown:
				mov dir1, 0     ; azuriraj smer na dole-desno
				
		 bounceLeft:                                                   
			mov dir2, 0          ; azuriraj smer ka levo
			cmp dir1, 1          ; vidi je l loptica isla na gore
			je bounceLeftUp      ; ako jeste, odbi je gore levo
			jne bounceLeftDown   ; odbi je dole levo


			
			bounceLeftUp:
				mov dir1 1      ; azuriraj smer na gore-levo
				jmp start
				
			bounceLeftDown:
				mov dir1, 0      ; azuriraj smer na dole-levo
				jmp start
				
		 bounceDown:                                                      
			mov dir1, 0           ; smer dole!
			cmp dir2, 1           ; uporedjuje da li se kretala na desno
			je bounceLeftDown     ; ako jeste, odskoci dole levo
			cmp dir2, 0           ; ako je dosla sa leve, odskoci dole desno
			je bounceRightDown    
			
			bounceLeftDown:          
				mov dir2, 0      ; azuriraj smer na levo
				jmp start
				
			bounceRightDown:      
				mov dir2, 1      ; azuriraj smer na desno
				jmp start
								  
		 bounceUp:                                                                                     
			mov dir1, 1           ; smer gore!
			cmp dir2, 1           ; da li je dosla iz desnog smera?
			je bounceLeftUp       ; odskoci gore levo, ako jeste.
			cmp dir2, 0           ; inache, ako je dosla sa leve
			je bounceRightUp      ; odskoci gore desno
			
			bounceLeftUp:
				mov dir2, 0       ; azuriraj smer za levo(gore)
				jmp start
				
			bounceRightUp:        ; azuriraj smer za desno(gore)
				mov dir2, 1
				jmp start
				
		 moveDown:
			cmp dir2, 1           ;da li je lopttica dosla sa desne strane?
			je moveDownRight      ;ako jeste, krece se dole desno
			cmp dir2, 0           ;ako je sa leve
			je moveDownLeft		  ;krece se dole levo
			
			moveDownRight:
				inc lopticaX               ;azurira kordinate loptice za dole-desno 
				inc lopticaY               
				jmp controlLimit
				
			moveDownLeft:
				inc lopticaX               ; uazurira kordinate loptice za dole-levo
				sub lopticaY, 1            
				jmp controlLimit
		   
		 moveUp:
			cmp dir2, 1           ; da li je loptica dosla sa desne strane?
			je moveUpRight		  ; ako jeste kreci se gore desno
			cmp dir2, 0           ; da li je loptica dosla sa desne strane?
			je moveUpLeft
			
			moveUpRight:
				sub lopticaX, 1            ; azuriraj kordinate za gore-desno
				inc lopticaY               
				jmp controlLimit
				
			moveUpLeft:
				sub lopticaX, 1            ; azuriraj kordinate za gore-levo
				sub lopticaY, 1            
				jmp controlLimit   
	  

    ret    
	
;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------;
    printPlatform:
	
        mov ah, 2h                   
        mov dh, platformaX                   
        mov dl, platformaY                
        int 10h                      
                                      
        mov ah, 9h                                      
        mov dx, offset platforma   
        mov bl, 4                                      
        int 21h
    ret

    printBlocks:
	
        mov dh, blokX                 
        mov dl, blokY
        call printLine
        
        mov dh, blokX
        add dh, 1                     
        mov dl, blokY
        call printLine
        
        mov dh, blokX
        add dh, 2                     
        mov dl, blokY
        call printLine
    ret
    
    printLine:
	
        mov ah, 2h                        
        int 10h 
          
        mov cx, 13                        
        loop1:                             
            mov ah, 9h                    
            mov dx, offset blok           
            int 21h                       
            loop loop1					
    ret

    printScore:                       
        mov ah, 2h
        mov dh, skorX                
        mov dl, skorY                 
        int 10h                       
                                      
        mov ah, 9h                    
        mov dx, offset skor        
        int 21h                       
                                     
        mov ah, 2h                    
        mov dh, scoreX                 
        mov dl, scoreY                
        add dl, 6                     
        int 10h                       
                                      
        mov al, inicijalniSkor         
        aam                          
        mov bx, ax                    
        mov ah, 02h                 
        mov dl, bh                     
        add dl, 30h                   
        int 21h                       
                                      
        mov ah, 02h                   
        mov dl, bl                   
        add dl, 30h                   
        int 21h                       
    ret                               
                      
    printLives:                                
        mov ah, 2h                        
        mov dh, zivotiX                    
        mov dl, zivotiY                    
        int 10h                           
        
	mov ah, 9h						  
        mov dx, offset zivoti 			  
        int 21h							  
        
	mov cx, 3                         
        loopN:                            
            mov ah, 9h                    
            mov dx, offset zivot          
            int 21h                       
            loop loopN                    
    ret                                   

    welcome:     
        mov ah, 2h
        mov dh, naslovX
        mov dl, naslovY
        int 10h
        
        mov ah, 9h
        mov dx, offset igrica
        int 21h
    ret
    
    clearScreen:
        mov AH, 6H 
        mov AL, 0    
        mov BH, 7         ;clear screen 
        mov CX, 0
        mov DL, 79
        mov DH, 24
        int 10H
    ret

end main 
cseg  ends
sseg segment stack 'STACK' 
     dw 64 dup(?)
sseg ends
