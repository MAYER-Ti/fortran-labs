CLASS (matrix(kind(0.0),10,10)) :: f
:
SELECT TYPE (ff => f)
	TYPE IS (matrix)
	: 
	TYPE IS (factored_matrix)
	: 
END SELECT

TYPE :: POINT
	REAL :: X, Y
END TYPE POINT
TYPE, EXTENDS(POINT) :: POINT_3D
	REAL :: Z
END TYPE POINT_3D
TYPE, EXTENDS(POINT) :: COLOR_POINT
	INTEGER :: COLOR
END TYPE COLOR_POINT

TYPE(POINT), TARGET :: P
TYPE(POINT_3D), TARGET :: P3
TYPE(COLOR_POINT), TARGET :: C
CLASS(POINT), POINTER :: P_OR_C

P_OR_C => C
SELECT TYPE ( A => P_OR_C )
TYPE IS ( POINT_3D )
	PRINT *, A%X, A%Y, A%Z
CLASS IS ( POINT )
	PRINT *, A%X, A%Y
END SELECT

class(*)