        program test_1

          ! Subroutine and Function declaration inside a program 

          implicit none

        contains
                SUBROUTINE    my_sub(x,y,i)
                IMPLICIT NONE
                   REAL :: x
                   REAL :: y 
                   INTEGER :: i
                   INTEGER :: j 
                   INTEGER :: li_ii

                   i = i + j
                   x = x + y
                   i = x

                   RETURN
                END SUBROUTINE my_sub
                ! ..............................

                ! ..............................
                FUNCTION    my_func(a,b,w)
                IMPLICIT NONE
                   INTEGER :: a
                   INTEGER :: b 
                   REAL :: w
                   ! LOCAL

                   RETURN
                END FUNCTION my_func
                ! ..............................


                ! ..............................
        end program test_1

