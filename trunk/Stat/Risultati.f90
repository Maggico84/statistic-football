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


SUBROUTINE Risultati(Squadra, Esito, Dimensione)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Lettura dei risultati e assegnazione del vettore esito.	!!
!! 1 = vittoria, 0 = pareggio, -1 = sconfitta 				!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

IMPLICIT NONE

!Variabili in Input
Integer :: Dimensione
Integer, dimension(Dimensione) :: Esito
Character(30), dimension(Dimensione) :: Squadra

!Variabili Locali
Integer :: i, j, Goalcasa, Goalfcasa
Character(30) :: Sqcasa, Sqfcasa

	open(10, file = 'risultati.dat')
	read(10, *) Sqcasa, Sqfcasa
	
	Esito = 0
	do i = 1, int(Dimensione/2)
		read(10, *) Sqcasa, Sqfcasa, Goalcasa, Goalfcasa
		do j = 1, Dimensione
			if(Squadra(j) .eq. Sqcasa) then
				if(Goalcasa .gt. Goalfcasa) then
					Esito(j) = 1
				else if(Goalcasa .lt. Goalfcasa) then
					Esito(j) = -1
				end if
			else if(Squadra(j) .eq. Sqfcasa) then
				if(Goalcasa .gt. Goalfcasa) then
					Esito(j) = -1
				else if(Goalcasa .lt. Goalfcasa) then
					Esito(j) = 1
				end if
			end if
		end do
	end do
	close(10)

END SUBROUTINE Risultati
