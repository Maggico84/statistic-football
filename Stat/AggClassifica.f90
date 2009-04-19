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


SUBROUTINE AggClassifica(Punti, Squadra, GareGiocate, Esito, ProbVittoria, Pdo1, Dimensione)

IMPLICIT NONE

!Variabili di Input-Output
Integer :: Dimensione
Integer, dimension(Dimensione) :: Punti, GareGiocate, Esito
Real(kind = 8), dimension(Dimensione) :: ProbVittoria, Pdo1
Character(30), dimension(Dimensione) :: Squadra

!Variabili Locali
Integer :: i, j, massimo, GGm, Em
Real(kind = 8) :: PVm, Pdm
Character(30) :: charmax

	do i = 1, Dimensione - 1
		massimo = Punti(i)
			charmax = Squadra(i)
				GGm = GareGiocate(i)
					Em = Esito(i)
						PVm = ProbVittoria(i)
							Pdm = Pdo1(i)
		do j = i + 1, Dimensione
			if(Punti(j) .gt. massimo ) then
				Punti(i) = Punti(j)
					Squadra(i) = Squadra(j)
						GareGiocate(i) = GareGiocate(j)
							Esito(i) = Esito(j)
								ProbVittoria(i) = ProbVittoria(j)
									Pdo1(i) = Pdo1(j)
				Punti(j) = massimo
					Squadra(j) = charmax
						GareGiocate(j) = GGm
							Esito(j) = Em
								ProbVittoria(j) = PVm
									Pdo1(j) = Pdm
				massimo = Punti(i)
					charmax = Squadra(i)
						GGm = GareGiocate(i)
							Em = Esito(i)
								PVm = ProbVittoria(i)
									Pdm = Pdo1(i)
			end if
		end do 
	end do

END SUBROUTINE
