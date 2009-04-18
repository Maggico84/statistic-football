!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!Riordinamento dei vettori real e char in ordine 	!!
!!crescente in base ai valori del vettore real		!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE Sort(VettoreReal, VettoreChar, Dimensione)

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
