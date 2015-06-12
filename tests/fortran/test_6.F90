        MODULE my_mod_1 

          ! multi Subroutine and Function declarations inside a MODULE 

          implicit none

        contains

                ! ..............................
                FUNCTION    my_func2(a)
                IMPLICIT NONE
                   INTEGER :: a
                   ! LOCAL

                   call my_sub2(w,a)
                   RETURN
                END FUNCTION my_func2
                ! ..............................


                ! ..............................
                SUBROUTINE    my_sub2(xx,ii)
                IMPLICIT NONE
                   REAL :: xx
                   INTEGER :: ii

                   ii = xx
                   call my_sub(xx,xx,ii)
                   my_func2(xx)

                   RETURN
                END SUBROUTINE my_sub2
                ! ..............................

                ! ..............................
                FUNCTION    my_func3(a)
                IMPLICIT NONE
                   INTEGER :: a
                   ! LOCAL

                   CONTAINS
                      ! ..............................
                      FUNCTION    my_func_inner(a)
                      IMPLICIT NONE
                         INTEGER :: a
                         ! LOCAL

                         call my_sub2(w,a)
                         RETURN
                      END FUNCTION my_func_inner
                      ! ..............................

                      ! ..............................
                      SUBROUTINE    my_sub_inner(xx,ii)
                      IMPLICIT NONE
                         REAL :: xx
                         INTEGER :: ii
                     
                         ii = xx
                         call my_sub(xx,xx,ii)
                         my_func2(xx)
                     
                         RETURN
                      END SUBROUTINE my_sub_inner
                      ! ..............................


                   RETURN
                END FUNCTION my_func3
                ! ..............................

                ! ..............................
        end MODULE my_mod_1 







        MODULE my_mod_2

          ! multi Subroutine and Function declarations inside a MODULE 

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
                   my_func2(x)

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

                   call my_sub(w,w,a)
                   my_func2(a)
                   RETURN
                END FUNCTION my_func
                ! ..............................


                ! ..............................
        end MODULE my_mod_2

