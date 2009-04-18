SUBROUTINE Risultati(Squadra, Esito, Dimensione)

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
