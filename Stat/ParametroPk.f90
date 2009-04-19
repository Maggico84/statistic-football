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


SUBROUTINE ParametroPk(Squadra, Punti, GareGiocate, ProbVittoria, Esito, ParamK, ParamEps, Dimensione)

IMPLICIT NONE

!Variabili di Input
Integer :: Dimensione
Integer, dimension(Dimensione) :: Punti, GareGiocate, Esito
Real(kind = 8), dimension(Dimensione) :: ProbVittoria
Character(30), dimension(Dimensione) :: Squadra
!Variabili di Output
Real(kind = 8), dimension(Dimensione) :: ParamK, ParamEps

!Variabili Locali
Integer :: i, j, camp, champ, uefa, retro
Integer, dimension(Dimensione) :: Pdo1, Pdo2, PartMancanti
Real(kind = 8) :: Kmax, Kmin, a, b

	!Calcolo i 4 obiettivi
	if(Dimensione .lt. 16) then
		camp  = Punti(1)
		champ = Punti(1)
		uefa  = Punti(1)
		retro = Punti(1)
	else
		camp  = Punti(1)
		champ = Punti(4)
		uefa  = Punti(7)
		retro = Punti(Dimensione - 3)	
	end if
	
	!Calcolo, per ogni squadra, i punti di distacco dall'obiettivo più vicino Pdo1
	do i = 1, Dimensione
		Pdo1(i) = abs(Punti(i) - camp)
		if(abs(Punti(i) - champ) .lt. Pdo1(i)) then
			Pdo1(i) = abs(Punti(i) - champ)
		end if
		if(abs(Punti(i) - uefa) .lt. Pdo1(i)) then
			Pdo1(i) = abs(Punti(i) - uefa)
		end if
		if(abs(Punti(i) - retro) .lt. Pdo1(i)) then
			Pdo1(i) = abs(Punti(i) - retro)
		end if
	end do
	
	!Aggiorno i punti di ogni squadra, tramite il vettore Esito
	do i = 1, Dimensione
		if(Esito(i) .eq. 1) then
			Punti(i) = Punti(i) + 3
		else if(Esito(i) .eq. 0) then
			Punti(i) = Punti(i) + 1
		end if
	end do
	
	!Aggiorno la classifica
	call AggClassifica(Punti, Squadra, GareGiocate, Esito, ProbVittoria, Pdo1, Dimensione)
	
	!Aggiorno i 4 obiettivi
	if(Dimensione .lt. 16) then
		camp  = Punti(1)
		champ = Punti(1)
		uefa  = Punti(1)
		retro = Punti(1)
	else
		camp  = Punti(1)
		champ = Punti(4)
		uefa  = Punti(7)
		retro = Punti(Dimensione - 3)	
	end if
	
	!Calcolo, per ogni squadra, i punti di distacco dall'obiettivo più vicino Pdo2
	do i = 1, Dimensione
		Pdo2(i) = abs(Punti(i) - camp)
		if(abs(Punti(i) - champ) .lt. Pdo2(i)) then
			Pdo2(i) = abs(Punti(i) - champ)
		end if
		if(abs(Punti(i) - uefa) .lt. Pdo2(i)) then
			Pdo2(i) = abs(Punti(i) - uefa)
		end if
		if(abs(Punti(i) - retro) .lt. Pdo2(i)) then
			Pdo2(i) = abs(Punti(i) - retro)
		end if
	end do	
	
	!Calcolo, per ogni squadra, le partite mancanti alla fine del campionato PartMancanti
	do i = 1, Dimensione
		PartMancanti(i) = 2*Dimensione - 2 - GareGiocate(i)
	end do
	
	!Calcolo i parametri ParamK ed ParamEps
	Kmax = 0.1
	kmin = 0.05
	
	!ParamPK
	do i = 1, Dimensione
		if(Pdo2(i) .eq. 0) then
			ParamK(i) = Kmax
		end if
		if(3*PartMancanti(i) .lt. Pdo2(i)) then
			ParamK(i) = Kmin
		else
			a = (Kmax * Kmin * 3 * PartMancanti(i)) / (Kmax - Kmin)
			b = (Kmin * 3 * PartMancanti(i)) / (Kmax - Kmin)
			ParamK(i) = a / (b + Pdo2(i))
		end if
	end do
	
	!ParamEps
	do i = 1, Dimensione
		if(Esito(i) .eq. 0) then
			if(Pdo1(i) .lt. Pdo2(i) .and. 3*PartMancanti(i) .ge. Pdo2(i)) then
				ParamEps(i) = ParamK(i) / 4.d0
			else if(Pdo1(i) .lt. Pdo2(i) .and. 3*PartMancanti(i) .lt. Pdo2(i)) then
				ParamEps(i) = - ParamK(i) / 2.d0
			else if(Pdo1(i) .ge. Pdo2(i) .and. 3*PartMancanti(i) .ge. Pdo2(i)) then
				ParamEps(i) = ParamK(i) / 2.d0
			else if(Pdo1(i) .ge. Pdo2(i) .and. 3*PartMancanti(i) .lt. Pdo2(i)) then
				ParamEps(i) = - ParamK(i) / 4.d0
			end if
		else
			ParamEps(i) = 0.d0
		end if
	end do
	
END SUBROUTINE ParametroPk
