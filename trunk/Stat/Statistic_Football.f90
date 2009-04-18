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

PROGRAM Statistic_Football

	IMPLICIT NONE
!Variabili
	Integer :: i, N, controllo
	Integer, dimension(:), allocatable :: Pti, PG
	Real(kind = 8) :: rangePV, rangeQ
	Real(kind = 8), dimension(:), allocatable :: PV, Q
	Character(30) :: Squadra, Punti, PartiteGiocate, ProbVittoria
	Character(30), dimension(:), allocatable :: Team
!Allocazione dei vettori
	N = 20
	allocate(Pti(N))
	allocate(PG(N))
	allocate(PV(N))
	allocate(Q(N))
	allocate(Team(N))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Controllo se nel file classifica.dat il vettore probabilità di vittoria è    !!
!diverso da zero, altrimenti utilizzo il file quote.dat			      !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	open(10, file = 'classifica.dat')
	read(10, *) Squadra, Punti, PartiteGiocate, ProbVittoria
	
	controllo = 0
	do i = 1, N
		read(10, *) Team(i), Pti(i), PG(i), PV(i)
		if(PV(i) .eq. 0.d0) then
			controllo = controllo + 1
		end if
	end do

	if(controllo .eq. N) then
		open(20, file = 'quote.dat')
		do i = 1, N
			read(20, *) Team(i), Q(i)
		end do			
		!riordinamento vettore Q e Team, ordine crescente
		!TO DO		
		call Sort(Q, Team, N)
		!inizializzazione dei valori di probabilità di vittoria		
		rangePV = 0.8d0
		rangeQ  = Q(N) - Q(1)
		do i = 1, N
			PV(i) = Q(i) * rangePV / rangeQ 		
		end do
	end if
	
	close(10)
	close(20)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Creo il vettore Effetto del pubblico con il file gare.dat e pubblico.dat		!!
!TO DO																			!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!	
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!Assegno le probabilità di vittoria degli incontri scrivendo in scommesse.dat  !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!Assegno i risultati ad ogni squadra											!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!Assegno la probabilità Pk, dovuta alla vittoria o meno di ogni squadra. Questa!! 
!!probabilità va ad aggiungersi o sottrarsi, alla probabiità di vittoria Q. In	!! 
!!caso di pareggio si ha una probabilità eps minore da aggiungere o sotrarre. 	!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!Aggiornamento della classifica												!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	deallocate(Pti)
	deallocate(PG)
	deallocate(PV)
	deallocate(Q)
	deallocate(Team)
END PROGRAM Statistic_Football