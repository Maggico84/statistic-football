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


SUBROUTINE Scommesse(Squadra, ProbVittoria, EffettoP, Dimensione)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!Funzione che assegna la probabilità di vittoria della partita	!!
!!ad ogni squadra. Tali probabilità vengono stampate sul file 	!!
!!scommesse.dat													!! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

IMPLICIT NONE
!Variabili in Input
Integer :: Dimensione
Real(kind = 8) :: EffettoP
Real(kind = 8), dimension(Dimensione) :: ProbVittoria
Character(30), dimension(Dimensione) :: Squadra 

!Variabili Locali
Integer :: i, j
Character(30) :: Sqcasa, Sqfcasa, NumTifSqcasa
Real(kind = 8) :: pv11, pv12, pv21, pv22

	open(10, file = 'gare.dat')
	read(10, *) Sqcasa, Sqfcasa, NumTifSqcasa
	
	open(20, file = 'scommesse.dat')
	write(20, *) 'Squadra_Casa', '', 'Squadra_Fcasa', '', '1', '', 'X', '', '2'
	
	do i = 1, int(Dimensione/2)
		read(10, *) Sqcasa, Sqfcasa, NumTifSqcasa
		do j = 1, Dimensione
			if(Squadra(j) .eq. Sqcasa) then
				pv11 = ProbVittoria(j) + EffettoP
				pv21 = 1 - pv11
			else if(Squadra(j) .eq. Sqfcasa) then
				pv12 = 1 - ProbVittoria(j)
				pv22 = ProbVittoria(j)
			end if
		end do
		write(20, *) Sqcasa, Sqfcasa, pv11 * pv12, pv21 * pv22, 1 - pv11*pv12 - pv21*pv22
	end do
	close(10)
	close(20)
	
END SUBROUTINE Scommesse
