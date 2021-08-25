PROGRAM test_reshape
    INTEGER, DIMENSION(4) :: x
    WRITE(6,*) SHAPE(x)                       ! prints "4"
    WRITE(6,*) SHAPE(RESHAPE(x, (/2, 2/)))    ! prints "2 2"
END PROGRAM
