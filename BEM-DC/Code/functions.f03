!*************************************************************************
!*                                                                      
!*     Computation of the value of the DC objective functions f1 and f2  
!*     and the correbonding subgradients (given by user).                
!*     Problems 1-14 are from DBDC aricle
!*     Problems 15-17 are from COAP aricle                                                          
!*     Called from subroutine bemdc (last modified 28.07.2022)            
!*                                                                       
!*************************************************************************
!*
!*     Modules included:
!*
!*     functions         
!*

MODULE functions                                         ! Computation of the value and the subgradient of the objective function.

    USE r_precision, ONLY : prec                      ! Precision for reals.
    USE param, ONLY : zero, one, two, large     ! Parameters.
    
    IMPLICIT NONE

    PUBLIC :: &
        init_x, &                                                   ! Starting point.
        myf1, &                                                    ! Computation of the value of the objective f1.
        myg1, &                                                   ! Computation of the subgradient of the objective f1.
        myf2                                                        ! Computation of the value of the objective f2.
       
        
CONTAINS

    SUBROUTINE init_x()                                 ! User supplied subroutine for initialization of vector x(n).
                                                                        ! Includes now starting points for test problems.
    
    USE initbem, ONLY : &
        n, &                                                         ! the number of variables.
        x, &                                                         ! the vector of variables. 
        sp                                                            ! Number of the starting point, sp = 1,...,5 in academic test examples.

        IMPLICIT NONE
        INTEGER :: i

        ! The starting points for the test problems   
        
        SELECT CASE(sp)          
         
!=======================================================================
            CASE(1)  
            
            x(1)=2.0_prec
            x(2)=2.0_prec
            x(1)=-7.2379_prec
            x(2)=8.0902_prec
!=======================================================================                             
            CASE(2)  
            
            x(1)=-1.2_prec
            x(2)=1.0_prec               
!=======================================================================                                            
            CASE(3)     
                 
            x(1)=1.0_prec
            x(2)=3.0_prec
            x(3)=3.0_prec
            x(4)=1.0_prec
!=======================================================================
            CASE(4)
            
            DO i = 1, n/2
               x(i)  = dble(i)
            END DO
            
            DO i = (n/2 + 1), n
               x(i)  = -dble(i)
            END DO  
!=======================================================================
            CASE(5)
            
            x = zero   
!=======================================================================
            CASE(6)
            
            x(1)=10.0_prec
            x(2)=1.0_prec
!=======================================================================
            CASE(7)
            
            x(1)=-2.0_prec
            x(2)=1.0_prec 
!=======================================================================
            CASE(8)
            
            x(1)=0.5_prec
            x(2)=0.5_prec
            x(3)=0.5_prec
!=======================================================================
            CASE(9)
            
            x(1)=4.0_prec
            x(2)=2.0_prec
            x(3)=4.0_prec
            x(4)=2.0_prec 
!=======================================================================
            CASE(10)
            
            DO i = 1, n
               x(i)  = 0.1* i
            END DO 
!=======================================================================  
            CASE(11)
            
            x(1)=10.0_prec
            x(2)=10.0_prec
            x(3)=10.0_prec        
!=======================================================================
            CASE(12)
            
            do i=1,n
               x(i)=2.0* i
            end do             
!=======================================================================                                        
            CASE(13)
            
            do i=1,n
              x(i)=10.0_prec
            end do 
!=======================================================================
            CASE(14)
            
            do i=1,n
              x(i)=1.0_prec
            end do             
!=======================================================================
            CASE(15)
            
            do i=1,n
              x(i)=3.0_prec
            end do             
!=======================================================================  
            CASE(16)
            
            do i=1,n
              x(i)=2.0_prec
            end do
!=======================================================================  
            CASE(17)
            
            do i=1,n
              x(i)=2.0_prec
            end do             
!=======================================================================  
            
        END SELECT        
        
    END SUBROUTINE init_x

    !************************************************************************
    !*                                                                      
    !*     * SUBROUTINE myf1 *                                             
    !*                                                                      
    !*     Computation of the value of the objective f1.                    
    !*                                                                      
    !************************************************************************
     
    SUBROUTINE myf1(n,x,f,iterm)

        USE param, ONLY : zero,one,two,large    ! Parameters.
        USE initbem, ONLY : pnf1                       ! Switch for convex component function f1 in academic test examples.
                                              

        IMPLICIT NONE

        ! Array Arguments
        REAL(KIND=prec), DIMENSION(n), INTENT(IN) :: &
            x                                                          ! Vector of variables.

        ! Scalar Arguments
        REAL(KIND=prec), INTENT(OUT) :: f       ! Value of the function.
        INTEGER, INTENT(IN) :: n                      ! Number of variables.
        INTEGER, INTENT(OUT) :: iterm             ! Cause of termination:
                                                                        !   0  - Everything is ok.
                                                                        !  -3  - Failure in function calculations
        REAL(KIND=prec) :: apu , largest            ! help variable                                        
        REAL(KIND=prec) :: a1,a2,a3,a4,a5,a6   ! help variables
        INTEGER :: i, j                                        ! help variable
        REAL(KIND=prec), DIMENSION(4) :: a            
          
                
        iterm = 0

        ! Function evaluation
        f = zero
        
!=======================================================================            
        SELECT CASE(pnf1)         
!=======================================================================
	    CASE(1)   
	
		 a1 = x(1)**4 + x(2)**2
		 a2 = (2.0_prec - x(1))**2 + (2.0_prec - x(2))**2 
		 a3 = 2.0_prec * EXP(-x(1)+x(2))
		
		a4 = x(1)**2 - 2.0_prec * x(1) + x(2)**2 - 4.0_prec * x(2) + 4.0_prec
		a5 = 2.0_prec * x(1)**2 - 5.0_prec * x(1) + x(2)**2 - 2.0_prec * x(2) 
		a5 = a5 + 4.0_prec
		a6 = x(1)**2 + 2.0_prec * x(2)**2 - 4.0_prec * x(2) + 1.0_prec               
		
		f = MAX(a1,a2,a3) + a4 + a5 + a6  
	
!=======================================================================
	   CASE(2)   
	
		IF ((ABS(x(1)) - x(2) ) > 0.0_prec) THEN
		f = ABS(x(1) - 1.0_prec) + 200.0_prec * (ABS(x(1)) - x(2) )
		ELSE
		f = ABS(x(1) - 1.0_prec)
		END IF
	
!=======================================================================
	   CASE(3)  
	
		f = ABS(x(1) - 1.0_prec)
		f = f + ABS(x(3) - 1.0_prec )                
		f = f + 4.95_prec * ( ABS(x(2) + x(4) - 2.0_prec ) )
		f = f + 10.1_prec * ( ABS(x(2)-1.0_prec) + ABS(x(4)-1.0_prec) )      
		
		IF ((ABS(x(1)) - x(2) ) > 0.0_prec) THEN
		f = f + 200.0_prec * (ABS(x(1)) - x(2) )
		END IF 
		
		IF ((ABS(x(3)) - x(4) ) > 0.0_prec) THEN
		f = f + 180.0_prec * (ABS(x(3)) - x(4) )
		END IF    
		
!=======================================================================                 
	   CASE(4)
	
		apu = ABS(x(1))
		DO i = 2, n
		IF (apu < ABS(x(i)) ) THEN 
		apu = ABS(x(i))
		END IF
		END DO
		f = n * apu  
!=======================================================================  
	   CASE(5)
	   
		apu = ABS(summa(x,1,n)) 
		DO j = 2, 20
		IF (apu <= ABS(summa(x,j,n)) ) THEN 
		apu = ABS(summa(x,j,n))
		END IF
		END DO
		f = 20.0_prec * apu
!=======================================================================
	   CASE(6)        
	
		f = x(2) + 0.1_prec * (x(1)**2 + x(2)**2)  
		
		IF (-x(2) <= 0.0_prec ) THEN 
		f = f
		ELSE
		f = f + 10.0_prec * (-x(2))
		END IF             
		
!=======================================================================
	   CASE(7)  
	                      
		f = ABS(x(1)-1.0_prec)+200.0_prec* MAX(0.0_prec, ABS(x(1))-x(2))                 
		
		a1 = x(1)**2 + x(2)**2 + ABS(x(2))
		a2 = x(1) + x(1)**2 + x(2)**2 + ABS(x(2)) - 0.5_prec
		a3 = ABS( x(1) - x(2) ) + ABS(x(2)) - 1.0_prec
		a4 = x(1) + x(1)**2 + x(2)**2
		
		f = f + 10.0_prec * MAX(a1,a2,a3,a4) 
	       
!=======================================================================
	CASE(8)  
	
		f = 9.0_prec - 8.0_prec * x(1) -6.0_prec * x(2) - 4.0_prec * x(3)  
		f = f + 2.0_prec * ABS(x(1)) + 2.0_prec * ABS(x(2))+ 2.0_prec * ABS(x(3)) 
		f = f + 4.0_prec * x(1)**2 + 2.0_prec * x(2)**2 + 2.0_prec * x(3)**2 
		
		a(1) = x(1) + x(2) + 2.0_prec * x(3) - 3.0_prec
		a(2) = -x(1)
		a(3) = -x(2)
		a(4) = -x(3)
		
		f = f + 10.0_prec * MAX(0.0_prec, a(1), a(2), a(3), a(4))        
	
!=======================================================================
	CASE(9)    
	                      
		f = x(1)**2 + (x(1)-1.0_prec)**2 + 2.0_prec*(x(1)-2.0_prec)**2 
		f = f + (x(1)-3.0_prec)**2 + 2.0_prec * x(2)**2 + (x(2)-1.0_prec)**2 
		f = f + 2.0_prec*(x(2)-2.0_prec)**2 + x(3)**2 + (x(3)-1.0_prec)**2 
		f = f + 2.0_prec*(x(3)-2.0_prec)**2 + (x(3)-3.0_prec)**2 
		f = f + 2.0_prec*x(4)**2 + (x(4)-1.0_prec)**2 + 2.0_prec*(x(4)-2.0_prec)**2         
	
!=======================================================================        
	CASE(10)
	
		f = zero
		DO i = 1, n
		f =  f + x(i)**2                             
		END DO       
	
!=======================================================================  
    CASE(11)          
                        
        f=4.0_prec*abs(x(1))+2.0_prec*abs(x(2))+2.0_prec*abs(x(3))-3.3d+01*x(1)+1.6d+01*x(2)-2.4d+01*x(3)
        f=f+1.0d+02*dmax1(0.0_prec,2.0_prec*abs(x(2))-3.0_prec*x(1)-7.0_prec)
        f=f+1.0d+02*dmax1(0.0_prec,abs(x(3))-4.0_prec*x(1)-1.1d+01)
!=======================================================================
    CASE(12)
            
      f=0.0_prec
	  do i=1,n
		f=f+abs(x(i))
	  end do
	  do i=1,n
	   f=f+1.0d+01*dmax1(0.0_prec,2.0_prec*(x(i)**2-x(i)-1.0_prec))
	  end do
!!=======================================================================
     CASE(13)  
                
     f=0.0_prec
      do i=1,n-1
        f=f+abs(x(i)+x(i+1))
      end do
      do i=1,n-2
        f=f+abs(x(i)+x(i+2))
      end do
       f=f+abs(x(1)+x(9))+abs(x(1)+x(10))+abs(x(2)+x(10))
       f=f+abs(x(1)+x(5))+abs(x(4)+x(7))
       apu =-1.0_prec
      do i=1,n
        apu =apu +x(i)
      end do
      if (apu.gt.0.0_prec) then
        f=f+10.0_prec*apu 
      end if

      do i=1,n
        if (-x(i).gt.0.0_prec) then
         f=f+10.0_prec*(-x(i))
        end if
      end do
        
!!=======================================================================
    CASE(14)   
               
         f = 0.0_prec
                     
		 apu = 0.0_prec
		 DO j =1, n
			apu = apu + x(j)/(1+j-1.0_prec)
		 END DO
		 
		 largest = ABS(apu) 
		 
		 DO i = 2,n
			apu = 0.0_prec
			DO j = 1, n
			  apu = apu + x(j)/(i+j-1.0_prec)
			END DO
			IF (ABS(apu) > largest) THEN 
			   largest = ABS(apu)
			END IF
		 END DO
		 
		 f = n * largest   
!=======================================================================  
    CASE(15)   

         f = 0.0_prec
         do i=1,n-1
          a1 = (x(i)-2.0_prec)**2+(x(i+1)-1.0_prec)**2
          a2 = exp(2.0_prec*x(i)-x(i+1))
          f =f+a1+a2
         end do
!======================================================================= 
    CASE(16)   

         a1=-1.0_prec
         do i=1,n
          a2=0.0_prec
          do j=1,n
           a2 = a2+x(j)**2/(i+j-1)
          end do
          a1 = MAX(a1,a2)
         end do
         f=n*a1
!======================================================================= 
    CASE(17)   

         a1=-1.0_prec
         do i=1,n
          a2=0.0_prec
          do j=1,n
           a2 = a2+x(j)/(i+j-1)
          end do
          a2=abs(a2)
          if(a2 > a1) then
           a1=a2
          end if       
         end do
         f=n*a1

        END SELECT
 
    
        RETURN
      
    END SUBROUTINE myf1

    !************************************************************************
    !*                                                                      
    !*     * SUBROUTINE myf2 *                                              
    !*                                                                      
    !*     Computation of the value of the objective f2.                    
    !*                                                                      
    !************************************************************************
     
    SUBROUTINE myf2(n,x,f,iterm)

        USE param, ONLY : zero,one,two,large    ! Parameters.
        USE initbem, ONLY : pnf2                       ! Switch for convex component function f2 in academic test examples.

        IMPLICIT NONE

        ! Array Arguments
        REAL(KIND=prec), DIMENSION(n), INTENT(IN) :: &
            x                                                          ! Vector of variables.

        ! Scalar Arguments
        REAL(KIND=prec), INTENT(OUT) :: f       ! Value of the function.
        INTEGER, INTENT(IN) :: n                      ! Number of variables.
        INTEGER, INTENT(OUT) :: iterm            ! Cause of termination:
                                                                        !   0  - Everything is ok.
                                                                        !  -3  - Failure in function calculations
        REAL(KIND=prec) :: apu, largest             ! help variable                                           
        REAL(KIND=prec) :: a2,a1,a4,a5,a6 
        INTEGER :: i, j, ind                                 ! help variable               
        REAL(KIND=prec), DIMENSION(n) :: term 
        
                
        iterm = 0

        ! Function evaluation
        f = zero
        SELECT CASE(pnf2)
        
!======================================================================= 
        CASE(1)               
        
	        a4 = x(1)**2 - 2.0_prec * x(1) + x(2)**2 - 4.0_prec * x(2) + 4.0_prec
	        a5 = 2.0_prec * x(1)**2 - 5.0_prec * x(1) + x(2)**2 - 2.0_prec * x(2) 
	        a5 = a5 + 4.0_prec
	        a6 = x(1)**2 + 2.0_prec * x(2)**2 - 4.0_prec * x(2) + 1.0_prec
	        
	        f = MAX( (a4 + a5) , (a5 + a6), (a4 + a6)  )    
        
!=======================================================================
        CASE(2)           
        
	        f = 100.0_prec * ( ABS(x(1)) - x(2) ) 
!=======================================================================
        CASE(3)  
        
	        f = 4.95_prec * ( ABS(x(2) - x(4)) )
	        f = f + 90.0_prec * ( ABS(x(3)) - x(4) )
	        f = f + 100.0_prec * ( ABS(x(1)) - x(2) ) 
!=======================================================================
        CASE(4)
        
	        f = zero
	        DO i = 1, n
	        f = f + ABS(x(i))                                   
	        END DO  
!=======================================================================
        CASE(5)
	        f = zero
	        DO j = 1, 20
	        f = f + ABS( summa(x,j,n) )
	        END DO 
!=======================================================================   
        CASE(6) 
        
	        f = 0.0_prec
	        
	        IF (x(1) <= 0.0_prec ) THEN 
	        f = f - x(1)
	        ELSE
	        f = f + x(1) 
	        END IF
	        
	        IF (x(2) <= 0.0_prec ) THEN 
	        f = f - x(2)
	        ELSE
	        f = f + x(2)
	        END IF    
	                                                           
!=======================================================================
        CASE(7)  
        
	        f = 10.0_prec * ( x(1)**2 + x(2)**2 + ABS(x(2)) )
	        f = f + 100.0_prec * ( ABS(x(1)) - x(2) ) 
	                   
!=======================================================================
        CASE(8)  
        
            f = ABS( x(1) -x(2) ) + ABS( x(1) -x(3) ) 
                 
!=======================================================================
        CASE(9) 
        
	        f = 0.0_prec 
	        
	        a1 = (x(1) -2.0_prec)**2 + x(2)**2
	        a2 = (x(3) -2.0_prec)**2 + x(4)**2
	        
	        IF ( a1 >= a2) THEN 
	        f = f + a1
	        ELSE 
	        f = f + a2
	        END IF
	        
	        a1 = (x(1) -2.0_prec)**2 + (x(2)-1.0_prec)**2
	        a2 = (x(3) -2.0_prec)**2 + (x(4)-1.0_prec)**2
	        
	        IF ( a1 >= a2) THEN 
	        f = f + a1
	        ELSE 
	        f = f + a2
	        END IF
	        
	        a1= (x(1) -3.0_prec)**2 + x(2)**2
	        a2= (x(3) -3.0_prec)**2 + x(4)**2
	        
	        IF ( a1 >= a2) THEN 
	        f = f + a1
	        ELSE 
	        f = f + a2
	        END IF
	        
	        a1 = (x(1))**2 + (x(2)-2.0_prec)**2
	        a2 = (x(3))**2 + (x(4)-2.0_prec)**2
	        
	        IF ( a1 >= a2) THEN 
	        f = f + a1
	        ELSE 
	        f = f + a2
	        END IF
	        
	        a1 = (x(1)-1.0_prec)**2 + (x(2)-2.0_prec)**2
	        a2 = (x(3)-1.0_prec)**2 + (x(4)-2.0_prec)**2
	        
	        IF ( a1 >= a2) THEN 
	        f = f + a1
	        ELSE 
	        f = f + a2
	        END IF      
	                              
!=======================================================================
        CASE(10)
        
	        f = zero
	        DO i = 2, n
	        f = f + ABS( x(i) - x(i-1) )
	        END DO   
	           
!=======================================================================
        CASE(11)
            
            f=2.0d+01*(-7.0_prec*x(1)+2.0_prec*abs(x(2))-abs(x(3))-1.8d+01)
        
!=======================================================================
       CASE(12)
 
		  f=0.0_prec
		  
		  do i=1,n
			f=f+10.0_prec*(x(i)**2-x(i)-1.0_prec)
		  end do
		  
		  term =0.0_prec
		  do i=1,n
			do j=1,n
			  if (j.ne.i) then
				term(i) =term(i) +abs(x(j))
			  end if
			end do
		  end do	
		 
		  largest = term(1)
          ind = 1 
                     
		  DO i = 2, n
			IF (term(i) > largest) THEN 
			   largest = term(i)
			   ind = i
			END IF
		  END DO
		 
		  f = f + largest	
			
!=======================================================================
    CASE(13) 
    
	      f=0.0_prec
	      do i=1,n-1
	        f=f+abs(x(i))+abs(x(i+1))
	      end do
	      do i=1,n-2
	        f=f+abs(x(i))+abs(x(i+2))
	      end do
	      f=f+3.0_prec*abs(x(1))+abs(x(2))+abs(x(4))+abs(x(5))+abs(x(7))
	      f=f +abs(x(9))+2.0_prec*abs(x(10))     
	         
!=======================================================================
    CASE(14) 
    
	     f=0.0_prec
	      do i=1,n
	        apu =0.0_prec
	        do j=1,n
	          apu =apu +x(j)/dble(i+j-1)
	        end do
	        f=f+abs(apu )
	      end do        
	      
!!=======================================================================
    CASE(15) 
    
	      f=0.0_prec
	      do i=1,n-1
           a1 = (x(i)-2.0_prec)**2+(x(i+1)-1.0_prec)**2
           a2 = exp(2.0_prec*x(i)-x(i+1))
           f=f+MAX(a1,a2)
          end do  
                
!!=======================================================================
    CASE(16) 
    
	      f=0.0_prec
	      do i=1,n
           a2=0.0_prec
           do j=1,n
            a2 = a2+x(j)**2/(i+j-1)
           end do
           f=f+a2
          end do    
              
!!=======================================================================
    CASE(17) 
    
	      f=0.0_prec
	      do i=1,n
           a2=0.0_prec
           do j=1,n
            a2 = a2+x(j)/(i+j-1)
           end do
           a2=abs(a2)
           f=f+a2
          end do  
                
!!=======================================================================

        END SELECT

       
        RETURN
      
    END SUBROUTINE myf2

    !************************************************************************
    !*                                                                      
    !*     * SUBROUTINE myg1 *                                             
    !*                                                                       
    !*     Computation of the subgradient of the function f1.               
    !*                                                                     
    !************************************************************************
     
    SUBROUTINE myg1(n,x,g,iterm)

        USE param, ONLY : zero,one,two,large    ! Parameters.
        USE initbem, ONLY : pnf1                       ! Switch for convex component function f1 in academic test examples.

        IMPLICIT NONE

        ! Array Arguments
        REAL(KIND=prec), DIMENSION(n), INTENT(IN) :: x    ! Vector of variables.
        REAL(KIND=prec), DIMENSION(n), INTENT(OUT) :: g ! Subgradient.

        ! Scalar Arguments
        INTEGER, INTENT(IN) :: n                      ! Number of variables.
        INTEGER, INTENT(OUT) :: iterm             ! Cause of termination:
                                                                        !   0  - Everything is ok.
                                                                        !  -3  - Failure in subgradient calculations
                                                                        !        (assigned by the user).

        REAL(KIND=prec) :: a2,a1,a3,a4              
        REAL(KIND=prec) :: apu                          ! help variable
        REAL(KIND=prec) :: largest                      ! help variable
        INTEGER :: i, j, ind, ka                            ! help variable
                
        
        REAL(KIND=prec), DIMENSION(4) :: a    
        REAL(KIND=prec), DIMENSION(5) :: b                     
        REAL(KIND=prec), DIMENSION(n) :: abs_sign                    

        iterm = 0

        SELECT CASE(pnf1)
        
!======================================================================= 
        CASE(0)
        
	        g = zero                
        
!=======================================================================    
        CASE(1) 
        
	        a1 = x(1)**4 + x(2)**2
	        a2 = (2.0_prec - x(1))**2 + (2.0_prec - x(2))**2 
	        a3 = 2.0_prec * EXP(-x(1)+x(2))              
	        
	        IF (a1 >= a2) THEN
	        IF (a1 >= a3) THEN
	        g(1) = 4.0_prec * x(1)**3 
	        g(2) = 2.0_prec * x(2)                    
	        ELSE
	        g(1) = -2.0_prec * EXP(-x(1)+x(2))
	        g(2) = 2.0_prec * EXP(-x(1)+x(2))                 
	        END IF               
	        ELSE
	        IF (a2 >= a3) THEN
	        g(1) = 2.0_prec * x(1) - 4.0_prec
	        g(2) = 2.0_prec * x(2) - 4.0_prec                   
	        ELSE
	        g(1) = -2.0_prec * EXP(-x(1)+x(2))
	        g(2) = 2.0_prec * EXP(-x(1)+x(2))                 
	        END IF                   
	        END IF
	        
	        g(1) = g(1) + 2.0_prec * x(1) - 2.0_prec
	        g(1) = g(1) + 4.0_prec * x(1) - 5.0_prec
	        g(1) = g(1) + 2.0_prec * x(1)
	        
	        g(2) = g(2) + 2.0_prec * x(2) - 4.0_prec
	        g(2) = g(2) + 2.0_prec * x(2) - 2.0_prec
	        g(2) = g(2) + 4.0_prec * x(2) - 4.0_prec         
        
!=======================================================================             
        CASE(2)  
        
	        IF ( x(1) <= 1.0_prec ) THEN
	        g(1) = -1.0_prec
	        ELSE
	        g(1) = 1.0_prec
	        END IF
	        
	        IF( ( ABS(x(1))-x(2) ) > 0.0_prec  ) THEN
	        IF(x(1) <= 0.0_prec) THEN 
	        g (1) = g (1) - 200.0_prec
	        ELSE
	        g (1) = g(1) + 200.0_prec  
	        END IF                   
	        g(2) = -200.0_prec
	        ELSE        
	        g(1) = g(1) 
	        g(2) = 0.0_prec
	        END IF  
	        
!=======================================================================    
        CASE(3)  
        
	        g(1) = 0.0_prec
	        g(2) = 0.0_prec
	        g(3) = 0.0_prec
	        g(4) = 0.0_prec
	        
	        IF ( x(1) <= 1.0_prec ) THEN
	        g(1) = -1.0_prec
	        ELSE
	        g(1) = 1.0_prec
	        END IF
	        
	        IF( ( ABS(x(1))-x(2) ) > 0.0_prec  ) THEN
	        IF(x(1) <= 0.0_prec) THEN 
	        g(1) = g(1) - 200.0_prec
	        ELSE
	        g(1) = g(1) + 200.0_prec  
	        END IF                    
	        g(2) = -200.0_prec
	        ELSE        
	        g(1) = g(1) 
	        g(2) = 0.0_prec
	        END IF
	        
	        IF( ( ABS(x(3))-x(4) ) > 0.0_prec  ) THEN
	        IF(x(3) <= 0.0_prec) THEN 
	        g(3) =  - 180.0_prec
	        ELSE
	        g(3) =  180.0_prec  
	        END IF                    
	        g(4) = -180.0_prec
	        ELSE        
	        g(3) = 0.0_prec 
	        g(4) = 0.0_prec
	        END IF
	        
	        IF ( x(3) <= 1.0_prec ) THEN
	        g(3) = g(3) - 1.0_prec
	        ELSE
	        g(3) = g(3) + 1.0_prec
	        END IF         
	        
	        IF ( x(2) <= 1.0_prec ) THEN
	        g(2) = g(2) - 10.1_prec  
	        ELSE
	        g(2) = g(2) + 10.1_prec
	        END IF         
	        
	        IF ( x(4) <= 1.0_prec ) THEN
	        g(4) = g(4) - 10.1_prec  
	        ELSE
	        g(4) = g(4) + 10.1_prec
	        END IF 
	        
	        IF ( (x(2) + x(4) - 2.0_prec) <= 0.0_prec ) THEN
	        g(2) = g(2) - 4.95_prec
	        g(4) = g(4) - 4.95_prec  
	        ELSE
	        g(2) = g(2) + 4.95_prec
	        g(4) = g(4) + 4.95_prec
	        END IF 
	        
!=======================================================================  
        CASE(4)
        
	        g = zero 
	        apu = ABS(x(1))
	        ind = 1
	        
	        DO i = 2, n
	        IF (apu < ABS(x(i))) THEN 
	        apu = ABS(x(i))
	        ind = i
	        END IF
	        END DO
	        
	        IF (x(ind) <= zero) THEN 
	        g(ind) = - n
	        ELSE 
	        g(ind) = n
	        END IF  
	                         
!=======================================================================    
        CASE(5)
        
	        g = zero 
	        apu = ABS(summa(x,1,n)) 
	        ind = 1
	        
	        DO j = 2, 20
	        IF (apu <= ABS(summa(x,j,n)) ) THEN 
	        apu = ABS(summa(x,j,n))
	        ind = j
	        END IF
	        END DO
	        
	        DO i = 1, n
	        g(i) = (0.05_prec * ind)**(i-1)
	        END DO             
	        
	        IF ( summa(x,ind,n) <= zero ) THEN 
	        g = -20.0_prec * g
	        ELSE
	        g = 20.0_prec * g
	        END IF 
        
!=======================================================================
        CASE(6)  
        
	        g = 0.0_prec                 
	        g(1) = 0.2_prec * x(1)                
	        g(2) = 1.0_prec + 0.2_prec * x(2) 
	        
	        IF ( -x(2) <= 0.0_prec ) THEN 
	        g(2) = g(2)
	        ELSE
	        g(2) = g(2) - 10.0_prec
	        END IF 
	        
!=======================================================================
        CASE(7)  
        
	        g = 0.0_prec                 
	        IF ( (x(1)-1.0_prec) <= 0.0_prec ) THEN 
	        g(1) = -1.0_prec
	        ELSE
	        g(1) = 1.0_prec
	        END IF 
	        
	        a(1) = x(1)**2 + x(2)**2 + ABS(x(2))
	        a(2) = x(1) + x(1)**2 + x(2)**2 + ABS(x(2)) - 0.5_prec
	        a(3) = ABS( x(1) - x(2) ) + ABS(x(2)) - 1.0_prec
	        a(4) = x(1) + x(1)**2 + x(2)**2                
	        
	        ind = 1
	        DO i = 2, 4
	        IF ( a(ind) < a(i) ) THEN 
	        ind = i
	        END IF 
	        END DO
	        
	        IF (ind == 1) THEN
	        g(1) = g(1) + 20.0_prec * x(1)
	        g(2) = g(2) + 20.0_prec * x(2)
	        IF (x(2) <= 0.0_prec) THEN
	        g(2) = g(2) - 10.0_prec
	        ELSE
	        g(2) = g(2) + 10.0_prec
	        END IF
	        END IF
	        
	        IF (ind == 2) THEN
	        g(1) = g(1) + 10.0_prec + 20.0_prec * x(1)
	        g(2) = g(2) + 20.0_prec * x(2)
	        IF (x(2) <= 0.0_prec) THEN
	        g(2) = g(2) - 10.0_prec
	        ELSE
	        g(2) = g(2) + 10.0_prec
	        END IF
	        END IF 
	        
	        IF (ind == 3) THEN
	        IF ( ( x(1) - x(2) ) <= 0.0_prec ) THEN
	        g(1) = g(1) - 10.0_prec 
	        g(2) = g(2) + 10.0_prec
	        ELSE
	        g(1) = g(1) + 10.0_prec
	        g(2) = g(2) - 10.0_prec                   
	        END IF
	        
	        IF (x(2) <= 0.0_prec) THEN
	        g(2) = g(2) - 10.0_prec                   
	        ELSE
	        g(2) = g(2) +   10.0_prec                 
	        END IF
	        END IF 
	        
	        IF (ind == 4) THEN
	        g(1) = g(1) + 10.0_prec + 20.0_prec * x(1)
	        g(2) = g(2) + 20.0_prec * x(2)
	        END IF
	        
	        IF ( (ABS(x(1)) - x(2) ) >= 0.0_prec ) THEN
	        g(2) = g(2) - 200.0_prec
	        IF ( x(1) <= 0.0_prec ) THEN
	        g(1) = g(1) - 200.0_prec
	        ELSE
	        g(1) = g(1) + 200.0_prec
	        END IF 
	        END IF                      
        
!=======================================================================
        CASE(8)  
        
	        g(1) = -8.0_prec + 8.0_prec * x(1)
	        g(2) = -6.0_prec + 4.0_prec * x(2)
	        g(3) = -4.0_prec + 4.0_prec * x(3)
	        
	        IF ( x(1) <= 0.0_prec ) THEN 
	        g(1) = g(1) - 2.0_prec
	        ELSE
	        g(1) = g(1) + 2.0_prec
	        END IF                 
	        
	        IF ( x(2) <= 0.0_prec ) THEN 
	        g(2) = g(2) - 2.0_prec
	        ELSE
	        g(2) = g(2) + 2.0_prec
	        END IF 
	        
	        IF ( x(3) <= 0.0_prec ) THEN 
	        g(3) = g(3) - 2.0_prec
	        ELSE
	        g(3) = g(3) + 2.0_prec
	        END IF                 
	        
	        b(1) = x(1) + x(2) + 2.0_prec * x(3) - 3.0_prec
	        b(2) = -x(1)
	        b(3) = -x(2)
	        b(4) = -x(3)
	        b(5) = 0.0_prec
	        
	        ind = 5
	        
	        DO i = 1, 4
	        IF ( b(ind) < b(i) ) THEN 
	        ind = i
	        END IF 
	        END DO
	        
	        IF (ind == 1) THEN
	        g(1) = g(1) + 10.0_prec 
	        g(2) = g(2) + 10.0_prec 
	        g(3) = g(3) + 20.0_prec 
	        END IF
	        
	        IF (ind == 2) THEN
	        g(1) = g(1) - 10.0_prec 
	        END IF 
	        
	        IF (ind == 3) THEN
	        g(2) = g(2) - 10.0_prec 
	        END IF 
	        
	        IF (ind == 4) THEN
	        g(3) = g(3) - 10.0_prec 
	        END IF 
	                        
!=======================================================================
        CASE(9)    
                             
	        g(1) = 10.0_prec * x(1) - 16.0_prec
	        g(2) = 10.0_prec * x(2) - 10.0_prec 
	        g(3) = 10.0_prec * x(3) - 16.0_prec
	        g(4) = 10.0_prec * x(4) - 10.0_prec   
	                            
!=======================================================================  
        CASE(10)
        
	        g = two * x   
	        
!=======================================================================
	    CASE(11)
	                  
		      g(1)=-33.0_prec
			  g(2)=16.0_prec
		      g(3)=-24.0_prec
		      
		      if (x(1).gt.0.0_prec) then
		         g(1)=g(1)+4.0_prec
		       else
		         g(1)=g(1)-4.0_prec
		      end if
		      
		      if (x(2).gt.0.0_prec) then
		         g(2)=g(2)+2.0_prec
		       else
		         g(2)=g(2)-2.0_prec
		      end if
		      
		      if (x(3).gt.0.0_prec) then
		         g(3)=g(3)+2.0_prec
		       else
		         g(3)=g(3)-2.0_prec
		      end if
		      
		      apu =2.0_prec*abs(x(2))-3.0_prec*x(1)-7.0_prec
		      
		      if (apu.ge.0.0_prec) then
		        g(1)=g(1)-300.0_prec
		        if (x(2).ge.0.0_prec)then
		           g(2)=g(2)+200.0_prec
		          else
		           g(2)=g(2)-200.0_prec
		        end if
		      end if
		      
		      apu=abs(x(3))-4.0_prec*x(1)-11.0_prec
		      if (apu.ge.0.0_prec) then
		        g(1)=g(1)-400.0_prec
		        if (x(3).ge.0.0_prec)then
		           g(3)=g(3)+100.0_prec
		          else
		           g(3)=g(3)-100.0_prec
		        end if
		      end if
		      
!=======================================================================
	    CASE(12) 
	 
		      do i=1,n
		         g(i)=0.0_prec
		       end do
		       do i=1,n
		         if (x(i).ge.0.0_prec) then
		             g(i)=g(i)+1.0_prec
		            else 
		             g(i)=g(i)-1.0_prec
		         end if
		       end do
		       do i=1,n
		         apu =2.0_prec*(x(i)**2-x(i)-1.0_prec)
		         if (apu .ge.0.0_prec) then
		            g(i)=g(i)+40.0_prec*x(i)-20.0_prec
		         end if
		       end do
		       
!=======================================================================		
	  CASE(13) 
	    
		     do i=1,n
		         g(i)=0.0_prec
		       end do
		       do i=1,n-1
		         if (x(i)+x(i+1).gt.0.0_prec) then
		           g(i)=g(i)+1.0_prec
		           g(i+1)=g(i+1)+1.0_prec
		           else
		           g(i)=g(i)-1.0_prec
		           g(i+1)=g(i+1)-1.0_prec
		         end if
		       end do
		       do i=1,n-2
		         if (x(i)+x(i+2).gt.0.0_prec) then
		           g(i)=g(i)+1.0_prec
		           g(i+2)=g(i+2)+1.0_prec
		           else
		           g(i)=g(i)-1.0_prec
		           g(i+2)=g(i+2)-1.0_prec
		         end if
		       end do
		       if (x(1)+x(9).gt.0.0_prec) then
		         g(1)=g(1)+1.0_prec
		         g(9)=g(9)+1.0_prec
		         else
		         g(1)=g(1)-1.0_prec
		         g(9)=g(9)-1.0_prec
		      end if
		      if (x(1)+x(10).gt.0.0_prec) then
		         g(1)=g(1)+1.0_prec
		         g(10)=g(10)+1.0_prec
		         else
		         g(1)=g(1)-1.0_prec
		         g(10)=g(10)-1.0_prec
		      end if
		      if (x(2)+x(10).gt.0.0_prec) then
		         g(2)=g(2)+1.0_prec
		         g(10)=g(10)+1.0_prec
		         else
		         g(2)=g(2)-1.0_prec
		         g(10)=g(10)-1.0_prec
		      end if
		      if (x(1)+x(5).gt.0.0_prec) then
		         g(1)=g(1)+1.0_prec
		         g(5)=g(5)+1.0_prec
		         else
		         g(1)=g(1)-1.0_prec
		         g(5)=g(5)-1.0_prec
		      end if
		      if (x(4)+x(7).gt.0.0_prec) then
		         g(4)=g(4)+1.0_prec
		         g(7)=g(7)+1.0_prec
		         else
		         g(4)=g(4)-1.0_prec
		         g(7)=g(7)-1.0_prec
		      end if
		      apu =-1.0_prec
		      do i=1,n
		        apu =apu +x(i)
		      end do
		      if (apu .gt.0.0_prec) then
		        do i=1,n
		          g(i)=g(i)+10.0_prec
		        end do
		      end if
		      do i=1,n
		        if (-x(i).gt.0.0_prec) then
		           g(i)=g(i)-10.0_prec
		        end if
		      end do   
	
!=======================================================================	   
		   CASE(14)
			 g = 0.0_prec
		   
			 apu = 0.0_prec
			 DO j =1, n
				apu = apu + x(j)/(1+j-1.0_prec)
			 END DO
			 IF (apu >= 0.0_prec) THEN 
				   abs_sign(1) = 1.0_prec
			 ELSE   
				   abs_sign(1) = -1.0_prec
			 END IF        
			 largest = ABS(apu) 
			 ind = 1
			 
			 DO i = 2, n
				apu = 0.0_prec
				DO j = 1, n
				  apu = apu + x(j)/(i+j-1.0_prec)
				END DO
				IF (apu >= 0.0_prec) THEN 
				   abs_sign(i) = 1.0_prec
				ELSE   
				   abs_sign(i) = -1.0_prec
				END IF
				IF (ABS(apu) > largest) THEN 
					largest = ABS(apu)
					ind = i
				END IF
			 END DO
			 
			 DO j = 1, n
				g(j) = abs_sign(ind) * n / (ind+j-1)
			 END DO    
         
!=======================================================================	   
		 CASE(15)
		 
		  g = 0.0_prec
		   
          g(1)=2.0_prec*(x(1)-2.0_prec)+2.0_prec*exp(2.0_prec*x(1)-x(2))
          g(n)=2.0_prec*(x(n)-1.0_prec)-exp(2.0_prec*x(n-1)-x(n))
          do i=2,n-1
           a1=2.0_prec*(x(i)-1.0_prec)-exp(2.0_prec*x(i-1)-x(i))
           a2=2.0_prec*(x(i)-2.0_prec)+2.0_prec*exp(2.0_prec*x(i)-x(i+1))
           g(i) = a1+a2
          end do
          
!=======================================================================
		 CASE(16)
		 
		  g = 0.0_prec
		   
          a1=-1.0_prec
          do i=1,n
           a2=0.0_prec
           do j=1,n
            a2 = a2+x(j)**2/(i+j-1)
           end do
           if(a2 > a1) then
            a1 = a2
            ka=i
           end if       
          end do

          do j=1,n
           g(j)=2.0_prec*n*x(j)/(ka+j-1)
          end do
          
!=======================================================================
		 CASE(17)
		 
		  g = 0.0_prec
		   
          a1=-1.0_prec
          do i=1,n
           a2=0.0_prec
           do j=1,n
            a2 = a2+x(j)/(i+j-1)
           end do
           a3=abs(a2)
           if(a3 > a1) then
            a1=a3
            a4=a2
            ka=i
           end if       
          end do

          if(a4 >= 0.0_prec) then
            do i=1,n
              g(i)=dble(n)/dble(ka+i-1)
            end do 
           else
            do i=1,n
              g(i)=-dble(n)/dble(ka+i-1)
            end do 
          end if  
!=======================================================================

        END SELECT                      

        RETURN

    END SUBROUTINE myg1
    

    !************************************************************************
    !*                                                                      
    !*     * SUBROUTINE myg2 *                                              
    !*                                                                      
    !*     Computation of the subgradient of the function f2.            
    !*                                                                      
    !************************************************************************
     
    SUBROUTINE myg2(n,x,g,iterm)

        USE param, ONLY : zero,one,two,large    ! Parameters.
        USE initbem, ONLY : pnf2                      ! Switch for convex component function  f2 in academic test examples.

        IMPLICIT NONE

        ! Array Arguments
        REAL(KIND=prec), DIMENSION(n), INTENT(IN) :: x    ! Vector of variables.
        REAL(KIND=prec), DIMENSION(n), INTENT(OUT) :: g ! Subgradient.

        ! Scalar Arguments
        INTEGER, INTENT(IN) :: n                      ! Number of variables.
        INTEGER, INTENT(OUT) :: iterm             ! Cause of termination:
                                                                         !   0  - Everything is ok.
                                                                         !  -3  - Failure in subgradient calculations
                                                                         !        (assigned by the user).

        REAL(KIND=prec) :: a2,a1,a4,a5,a6                
        REAL(KIND=prec) :: apu                          ! help variable
        REAL(KIND=prec) :: largest                      ! help variable
        INTEGER :: i, j, ind                                  ! help variable
        REAL(KIND=prec), DIMENSION(n) :: term 
                                         
        iterm = 0

        SELECT CASE(pnf2)
!=======================================================================
                
    CASE(0)
        g = zero
        
!=======================================================================
	CASE(1)                   
	
		g = 0.0_prec  
		
		a4 = x(1)**2 - 2.0_prec * x(1) + x(2)**2 - 4.0_prec * x(2) + 4.0_prec
		a5 = 2.0_prec * x(1)**2 - 5.0_prec * x(1) + x(2)**2 - 2.0_prec * x(2) 
		a5 = a5 + 4.0_prec
		a6 = x(1)**2 + 2.0_prec * x(2)**2 - 4.0_prec * x(2) + 1.0_prec               
		
		IF ( (a4 + a5) >= (a4 + a6) ) THEN
		IF ( (a4 + a5) >= (a5 + a6)) THEN
		g(1) = 6.0_prec * x(1) - 7.0_prec
		g(2) = 4.0_prec * x(2) - 6.0_prec
		ELSE
		g(1) = 6.0_prec * x(1) - 5.0_prec
		g(2) = 6.0_prec * x(2) - 6.0_prec
		END IF              
		ELSE
		IF ( (a4 + a6) >= (a5 + a6) ) THEN
		g(1) = 4.0_prec * x(1) - 2.0_prec
		g(2) = 6.0_prec * x(2) - 8.0_prec
		ELSE
		g(1) = 6.0_prec * x(1) - 5.0_prec
		g(2) = 6.0_prec * x(2) - 6.0_prec 
		END IF                  
		END IF 
		
!=======================================================================     
	CASE(2)             
	
		IF(x(1) <= 0) THEN
		g(1) = -100.0_prec 
		ELSE
		g(1) = 100.0_prec
		END IF
		
		g(2) = -100.0_prec    
		             
!=======================================================================
	CASE(3)  
	
		IF(x(1) <= 0.0_prec) THEN
		g(1) = -100.0_prec 
		ELSE
		g(1) = 100.0_prec
		END IF
		
		g(2) = -100.0_prec
		
		IF(x(3) <= 0.0_prec) THEN
		g(3) = -90.0_prec 
		ELSE
		g(3) = 90.0_prec
		END IF
		
		g(4) = -90.0_prec             
		
		IF( (x(2) - x(4) ) <= 0.0_prec) THEN
		g(2) = g(2) - 4.95_prec 
		g(4) = g(4) + 4.95_prec
		ELSE
		g(2) = g(2) + 4.95_prec 
		g(4) = g(4) - 4.95_prec                 
		END IF  
		
!=======================================================================	
	CASE(4)
	
		g = zero
		DO i = 1, n
		IF (x(i) <= zero ) THEN 
		g(i) = -one
		ELSE
		g(i) = one
		END IF
		END DO
!=======================================================================		
	CASE(5)
		g = zero 
		
		DO j = 1, 20
		IF (summa(x,j,n) <= zero) THEN 
		DO i = 1, n
		g(i) = g(i) - (0.05_prec * j)**(i-1)
		END DO                  
		ELSE 
		DO i = 1, n
		g(i) = g(i) + (0.05_prec * j)**(i-1)
		END DO                   
		END IF
		END DO  
	
!=======================================================================
	CASE(6)   
		
	DO i = 1, n
	IF (x(i) <= 0.0_prec ) THEN 
	g(i) = -1.0_prec
	ELSE
	g(i) = 1.0_prec
	END IF
	END DO   
	
!=======================================================================
	CASE(7)  
	
		g = 0.0_prec
		
		IF (x(2) <= 0.0_prec ) THEN 
		g(2) = 20.0_prec * x(2) - 10.0_prec 
		ELSE
		g(2) = 20.0_prec * x(2) + 10.0_prec 
		END IF
		
		g(2) = g(2) - 100.0_prec
		g(1) = 20.0_prec * x(1)
		
		IF (x(1) <= 0.0_prec ) THEN 
		g(1) = g(1) - 100.0_prec
		ELSE
		g(1) = g(1) + 100.0_prec
		END IF
	
!=======================================================================
	CASE(8)                      
	
		g = 0.0_prec                
		IF ( (x(1)-x(2)) <= 0.0_prec ) THEN 
		g(1) = g(1) - 1.0_prec 
		g(2) = g(2) + 1.0_prec
		ELSE
		g(1) = g(1) + 1.0_prec
		g(2) = g(2) - 1.0_prec 
		END IF
		
		IF ( (x(1)-x(3)) <= 0.0_prec ) THEN 
		g(1) = g(1) - 1.0_prec 
		g(3) = g(3) + 1.0_prec
		ELSE
		g(1) = g(1) + 1.0_prec
		g(3) = g(3) - 1.0_prec     
		END IF         
	
!=======================================================================
	CASE(9)   
	
		g = 0.0_prec 
		
		a1 = (x(1) -2.0_prec)**2 + x(2)**2
		a2 = (x(3) -2.0_prec)**2 + x(4)**2
		
		IF ( a1 >= a2) THEN 
		g(1) = g(1) + 2.0_prec * (x(1)-2.0_prec)
		g(2) = g(2) + 2.0_prec * x(2)
		ELSE 
		g(3) = g(3) + 2.0_prec * (x(3)-2.0_prec)
		g(4) = g(4) + 2.0_prec * x(4)
		END IF
		
		a1 = (x(1) -2.0_prec)**2 + (x(2)-1.0_prec)**2
		a2 = (x(3) -2.0_prec)**2 + (x(4)-1.0_prec)**2
		
		IF ( a1 >= a2) THEN 
		g(1) = g(1) + 2.0_prec * (x(1)-2.0_prec)
		g(2) = g(2) + 2.0_prec * (x(2)-1.0_prec)
		ELSE 
		g(3) = g(3) + 2.0_prec * (x(3)-2.0_prec)
		g(4) = g(4) + 2.0_prec * (x(4)-1.0_prec)
		END IF
		
		a1 = (x(1) -3.0_prec)**2 + x(2)**2
		a2 = (x(3) -3.0_prec)**2 + x(4)**2
		
		IF ( a1 >= a2) THEN 
		g(1) = g(1) + 2.0_prec * (x(1)-3.0_prec)
		g(2) = g(2) + 2.0_prec * (x(2))
		ELSE 
		g(3) = g(3) + 2.0_prec * (x(3)-3.0_prec)
		g(4) = g(4) + 2.0_prec * (x(4)) 
		END IF
		
		a1 = (x(1))**2 + (x(2)-2.0_prec)**2
		a2 = (x(3))**2 + (x(4)-2.0_prec)**2
		
		IF ( a1 >= a2) THEN 
		g(1) = g(1) + 2.0_prec * (x(1))
		g(2) = g(2) + 2.0_prec * (x(2)-2.0_prec)
		ELSE 
		g(3) = g(3) + 2.0_prec * (x(3))
		g(4) = g(4) + 2.0_prec * (x(4)-2.0_prec)
		END IF
		
		a1 = (x(1)-1.0_prec)**2 + (x(2)-2.0_prec)**2
		a2 = (x(3)-1.0_prec)**2 + (x(4)-2.0_prec)**2
		
		IF ( a1 >= a2) THEN 
		g(1) = g(1) + 2.0_prec * (x(1)-1.0_prec)
		g(2) = g(2) + 2.0_prec * (x(2)-2.0_prec)
		ELSE 
		g(3) = g(3) + 2.0_prec * (x(3)-1.0_prec)
		g(4) = g(4) + 2.0_prec * (x(4)-2.0_prec)
		END IF                        
	
!=======================================================================
	CASE(10)
	
		g = zero
				
		DO i = 2, n
		IF ( x(i) - x(i-1) <= zero) THEN
		g(i-1) = g(i-1) + one 
		g(i) = g(i) - one 
		ELSE
		g(i-1) = g(i-1) - one  
		g(i) = g(i) + one     
		END IF
		END DO   
		
!======================================================================= 
	 CASE(11)
                  
	      g(1)=-140.0_prec
	      g(2)=0.0_prec
	      g(3)=0.0_prec
	      if (x(2).ge.0.0_prec) then
	         g(2)=40.0_prec
	        else
	         g(2)=-40.0_prec
	      end if
	      if (x(3).ge.0.0_prec) then
	         g(3)=-20.0_prec
	        else
	         g(3)=20.0_prec
	      end if          

!=======================================================================
     CASE(12)
 
	      do i=1,n
	        g(i)=0.0_prec
	      end do
	      
	      do i=1,n
	        g(i)=g(i)+20.0_prec*x(i)-10.0_prec
	      end do
	      
          term=0.0_prec
          
	       do i=1,n
	        do j=1,n
	          if (j.ne.i) then
	            term(i)=term(i)+abs(x(j))
	          end if
	        end do
	        end do
	        
	        largest = term(1)
	        ind = 1
	        
	        DO i = 2, n
				IF (term(i) > largest) THEN 
				   largest = term(i)
				   ind = i
				END IF
		   END DO    
      	     	           
	      do i=1,n
	        if (i.ne.ind) then
	          if (x(i).gt.0.0_prec) then
	            g(i)=g(i)+1.0_prec
	            else
	            g(i)=g(i)-1.0_prec
	          end if
	        end if
	      end do            
	
!=======================================================================
     CASE(13)
    
	     do i=1,n
	        g(i)=0.0_prec
	      end do
	      do i=1,n-1
	        if (x(i).ge.0.0_prec) then
	          g(i)=g(i)+1.0_prec
	          else
	          g(i)=g(i)-1.0_prec
	        end if
	      end do
	      do i=1,n-1
	        if (x(i+1).ge.0.0_prec) then
	          g(i+1)=g(i+1)+1.0_prec
	          else
	          g(i+1)=g(i+1)-1.0_prec
	        end if
	      end do
	      do i=1,n-2
	        if (x(i).ge.0.0_prec) then
	          g(i)=g(i)+1.0_prec
	          else
	          g(i)=g(i)-1.0_prec
	        end if
	      end do
	      do i=1,n-2
	        if (x(i+2).ge.0.0_prec) then
	          g(i+2)=g(i+2)+1.0_prec
	          else
	          g(i+2)=g(i+2)-1.0_prec
	        end if
	      end do
	      if (x(1).ge.0.0_prec) then
	          g(1)=g(1)+3.0_prec
	          else
	          g(1)=g(1)-3.0_prec
	      end if
	      if (x(2).ge.0.0_prec) then
	          g(2)=g(2)+1.0_prec
	          else
	          g(2)=g(2)-1.0_prec
	      end if
	      if (x(4).ge.0.0_prec) then
	          g(4)=g(4)+1.0_prec
	          else
	          g(4)=g(4)-1.0_prec
	      end if
	      if (x(5).ge.0.0_prec) then
	          g(5)=g(5)+1.0_prec
	          else
	          g(5)=g(5)-1.0_prec
	      end if
	      if (x(7).ge.0.0_prec) then
	          g(7)=g(7)+1.0_prec
	          else
	          g(7)=g(7)-1.0_prec
	      end if
	      if (x(9).ge.0.0_prec) then
	          g(9)=g(9)+1.0_prec
	          else
	          g(9)=g(9)-1.0_prec
	      end if
	      if (x(10).ge.0.0_prec) then
	          g(10)=g(10)+2.0_prec
	          else
	          g(10)=g(10)-2.0_prec
	      end if  

!=======================================================================       
       CASE(14)
    
	      do i=1,n
	        g(i)=0.0_prec
	      end do  
	        
	      do i=1,n
	        apu =0.0_prec
	        do j=1,n
	          apu =apu +x(j)/dble(i+j-1)
	        end do
	        if (apu.ge.0.0_prec) then
	           do j=1,n
	             g(j)=g(j)+1.0_prec/dble(i+j-1)
	           end do
	         else
	           do j=1,n
	             g(j)=g(j)-1.0_prec/dble(i+j-1)
	           end do
	        end if
	      end do         
		                  
!=======================================================================                    
		 CASE(15)
		  g = 0.0_prec
		   
          a1 = (x(1)-2.0_prec)**2+(x(2)-1.0_prec)**2
          a2 = exp(2.0_prec*x(1)-x(2))

          if (a1 >= a2) then
              g(1)=2.0_prec*(x(1)-2.0_prec)
            else
              g(1)=2.0_prec*exp(2.0_prec*x(1)-x(2))
          end if

          a1 = (x(n-1)-2.0_prec)**2+(x(n)-1.0_prec)**2
          a2 = exp(2.0_prec*x(n-1)-x(n))

          if (a1 >= a2) then
              g(n)=2.0_prec*(x(n)-1.0_prec)
            else
              g(n)=-exp(2.0_prec*x(n-1)-x(n))
          end if

          do i=2,n-1

            a1 = (x(i-1)-2.0_prec)**2+(x(i)-1.0_prec)**2
            a2 = exp(2.0_prec*x(i-1)-x(i))
       
            a5 = (x(i)-2.0_prec)**2+(x(i+1)-1.0_prec)**2
            a6 = exp(2.0_prec*x(i)-x(i+1))
       
            if (a1 >= a2) then
                g(i) = 2.0_prec*(x(i)-1.0_prec)
              else
                g(i) = -exp(2.0_prec*x(i-1)-x(i))
            end if
            if (a5 >= a6) then
                g(i) = g(i)+2.0_prec*(x(i)-2.0_prec)
              else
                g(i) = g(i)+2.0_prec*exp(2.0_prec*x(i)-x(i+1))
            end if
          end do
          
!=======================================================================
       CASE(16)
    
          do j=1,n
           g(j)=0.0_prec
           do i=1,n
             g(j)=g(j)+2.0_prec*x(j)/(j+i-1)
           end do
          end do
!=======================================================================   
       CASE(17)
    
	      do i=1,n
	        g(i)=0.0_prec
	      end do  
          do i=1,n
           a2=0.0_prec
           do j=1,n
             a2 = a2+x(j)/(i+j-1)
           end do          
           if(a2 >= 0.0_prec) then
              do j=1,n
               g(j)=g(j)+1.0_prec/(i+j-1)
              end do
            else
              do j=1,n
               g(j)=g(j)-1.0_prec/(i+j-1) 
              end do 
           end if  
          end do
!=======================================================================  

        END SELECT                      
        RETURN

    END SUBROUTINE myg2
    

    FUNCTION summa(y, j, n) RESULT(f)           
        !
        ! Calculates the sum used in f_1 and f_2 for parameter t_j.
        ! NOTICE: The dimension of 'y' has to be 'n'. 'j' needs to be an integer from interval [1,20]
        !
        USE param, ONLY : zero,one                    ! Parameters.
        
        IMPLICIT NONE
                
        REAL(KIND=prec), DIMENSION(:), INTENT(IN) :: y  ! a point where the function value of the DC component is calculated
        INTEGER, INTENT(IN) ::  j                                       ! determines the parameter t_j
        INTEGER, INTENT(IN) ::  n                                      ! dimension of the problem
                
        REAL(KIND=prec) :: f, t, apu                    
        INTEGER :: i                                    
                
        f = zero
        apu = one/n
                
        DO i = 1, n 
            t = (0.05_prec * j )**(i-1)
            f = f + (y(i) - apu)*t            
        END DO 
            
    END FUNCTION summa
    

END MODULE functions

