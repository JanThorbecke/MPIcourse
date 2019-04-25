      program dprod
      parameter (n=20000)
      parameter (nrep=100)
      real a(n,n), sum

      do 10 i = 1, n
         do 11 j = 1, n
            a(j,i) = (i * j) / (i + j)
 11      continue
 10   continue

      print *, ' Initialization done, entering computations'

      sum = 0.0

      do 20 k = 1, nrep
         sum = sum + prod(k,n,a)
 20   continue

      print *, ' Done, SUM = ', sum/nrep

      end

      function prod(k,n,a)

      integer n
      real a(n,n)

      prod = 0.0

c$omp parallel do
c$omp&   shared(k,n,a,prod)
c$omp&   private(i,j)
      do 30 i = 1, n
         do 31 j = 2, n

            a(1,i) = a(1,i) + a(j,i)/k

 31      continue

         prod = prod + a(1,i)

 30   continue
c$omp end parallel do

      return
      end
