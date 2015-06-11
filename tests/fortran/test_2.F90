        program test_2

          ! multi Subroutine and Function declarations inside a program 

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
                FUNCTION    my_func2(a)
                IMPLICIT NONE
                   INTEGER :: a
                   ! LOCAL

                   RETURN
                END FUNCTION my_func2
                ! ..............................


                ! ..............................
                SUBROUTINE    my_sub2(xx,ii)
                IMPLICIT NONE
                   REAL :: xx
                   INTEGER :: ii

                   ii = xx

                   RETURN
                END SUBROUTINE my_sub2
                ! ..............................

                ! ..............................
                FUNCTION    my_func3(a)
                IMPLICIT NONE
                   INTEGER :: a
                   ! LOCAL

                   RETURN
                END FUNCTION my_func3
                ! ..............................

                ! ..............................
        end program test_2

