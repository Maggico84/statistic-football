!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Copyright (c) 2009, Davide Ferrarese									      !!
!! All rights reserved.							 						      !!
!! 									     									  !!
!! Redistribution and use in source and binary forms, with or without         !!
!! modification, are permitted provided that the following conditions are met:!!
!! 																		      !!
!! 1. Redistributions of source code must retain the above copyright notice,  !!
!!    this list of conditions and the following disclaimer.                   !!
!! 2. Redistributions in binary form must reproduce the above copyright       !!
!!    notice, this list of conditions and the following disclaimer in the     !!
!!    documentation and/or other materials provided with the distribution.    !!
!! 																		      !!
!! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"!!
!! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE  !!
!! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE !!
!! ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE   !!
!! LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR        !!
!! CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF       !!
!! SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS   !!
!! INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN    !!
!! CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)    !!
!! ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE !!
!! POSSIBILITY OF SUCH DAMAGE.                                                !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE Sort(VettoreReal, VettoreChar, Dimensione)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!Riordinamento dei vettori real e char in ordine 	!!
!!crescente in base ai valori del vettore real		!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

IMPLICIT NONE

!Variabili in Input
Integer :: Dimensione
Real(Kind = 8), dimension(Dimensione) :: VettoreReal
Character(30), dimension(Dimensione) :: VettoreChar

!Variabili Locali
Integer :: i, j
Real(kind = 8) :: minimo
Character(30) :: charmin

	do i = 1, Dimensione-1
		minimo = VettoreReal(i)
		charmin = VettoreChar(i)
		do j = i+1, Dimensione
			if(VettoreReal(j) .lt. minimo ) then
				VettoreReal(i) = VettoreReal(j)
					VettoreChar(i) = VettoreChar(j)
				VettoreReal(j) = minimo
					VettoreChar(j) = charmin
				minimo = VettoreReal(i)
					charmin = VettoreChar(i)
			end if
		end do 
	end do

END SUBROUTINE Sort
