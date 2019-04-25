      program test
      parameter(l=2000,m=2000,n=2000)

      REAL*4 A(l,m),B(m,n),C(l,n)
 
      do 10 j=1,m
         do 11 i=1,l
            A(i,j) = i
 11      continue
 10   continue
 
      do 20 j=1,n
         do 21 i=1,m
            B(i,j) = 0.5
 21      continue
         B(i,i) = 1.0
 20   continue

      print *,"A and B initialized. Entering repetition loop"
    
      do 40 itime=1,30
         call matmul(A,B,C,l,m,n)
 40   continue

      print *,"Done, C(200,201) = ", C(200,201)
    
      end


      SUBROUTINE MATMUL(A,B,C,l,m,n)

      INTEGER l,m,n,i,j,k

      REAL*4 A(l,m),B(m,n),C(l,n)

      do 30 i = 1,l
         do 31 j = 1,n

            C(i,j) = 0.0E+00

            do 32 k = 1,m
               C(i,j)=C(i,j)+A(i,k)* B(k,j)
 32         continue

 31      continue
 30   continue

      return

      end 

