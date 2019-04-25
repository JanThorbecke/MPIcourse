      program ex3
      parameter (n=1200)
      parameter (m=5000)
      real a(n,n), b(n,n), c(n,n)

      do 10 i = 1, n
         do 11 j = 1, n
     	         a(j,i) = (i * j) / (i + j)
		 b(j,i) = 0.
		 c(j,i) = 0.
 11      continue
 10   continue

c$omp  parallel do
c$omp& shared(a,b,c)
c$omp& private(i,j,k)
c$omp& schedule(dynamic,1)
      do 20 i = 2, n-1
         do 21 j = 2, i
            do 22 k = 1, m
		    b(j,i) = b(j,i)
     &                       + a(j  ,i-1)/k     + a(j  ,i+1)/k
     &		             + a(j-1,i  )/k     + a(j+1,i  )/k
     &                       + a(j-1,i-1)/(k*k) + a(j+1,i+1)/(k*k)
     &                       + a(j-1,i+1)/(k*k) + a(j+1,i-1)/(k*k)
 22         continue
 21      continue
 20   continue
c$omp  end parallel do

c$omp  parallel do
c$omp& shared(a,b,c)
c$omp& private(i,j,k)
c$omp& schedule(runtime)
      do 30 i = 2, n-1
         do 31 j = 2, i
            do 32 k = 1, m
		    c(j,i) = c(j,i)
     &                       + a(j  ,i-1)/k     + a(j  ,i+1)/k
     &		             + a(j-1,i  )/k     + a(j+1,i  )/k
     &                       - a(j-1,i-1)/(k*k) - a(j+1,i+1)/(k*k)
     &                       - a(j-1,i+1)/(k*k) - a(j+1,i-1)/(k*k)
 32         continue
 31      continue
 30   continue
c$omp  end parallel do


      print *, ' Done, B(50,51), C(50,51) is: ', b(50,51),c(50,51)

      end
