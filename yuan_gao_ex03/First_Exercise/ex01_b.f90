program factorial_calc
      implicit none
      integer::n,factn
      n = 7
      factn = 1
      call factsub(n,factn)
end program factorial_calc

subroutine factsub(n,factn)
  implicit none
  integer::factn,n,i1,istep,i

  parameter (i1=1,istep=1)
  do i = i1,n,istep
    factn=factn*i
  enddo
  print*,"the number n = 7 and its factorial ="
  print*,factn
  stop
end subroutine

