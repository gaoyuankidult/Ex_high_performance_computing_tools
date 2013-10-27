module complex_ope
  implicit none
  integer,parameter::p = -1
  type complex_num
     real::real_part
     real::image_part
  end type complex_num
  interface operator(+)
     module procedure opfunc_plus
  end interface operator(+)
  interface operator(-)
     module procedure opfunc_minus
  end interface operator(-)
  interface operator(*)
     module procedure opfunc_multi
  end interface operator(*)
  interface operator(/)
     module procedure opfunc_div
  end interface operator(/)
contains
  function opfunc_plus(c1,c2)
    implicit none
    type(complex_num) :: opfunc_plus
    type(complex_num),intent(in) :: c1,c2
    opfunc_plus%real_part = c1%real_part+c2%real_part
    opfunc_plus%image_part = c1%image_part + c2%image_part
    return
  end function opfunc_plus
  function opfunc_minus(c1,c2)
    implicit none
    type(complex_num) :: opfunc_minus
    type(complex_num),intent(in) :: c1,c2
    opfunc_minus%real_part = c1%real_part-c2%real_part
    opfunc_minus%image_part = c1%image_part - c2%image_part
    return
  end function opfunc_minus
  function opfunc_multi(c1,c2)
    implicit none
    type(complex_num) :: opfunc_multi
    type(complex_num),intent(in) :: c1,c2
    opfunc_multi%real_part = c1%real_part*c2%real_part + p * c1%image_part * c2%image_part 
    opfunc_multi%image_part = c1%image_part * c2%real_part + c2%image_part * c1%real_part
    return
  end function opfunc_multi
  function opfunc_div(c1,c2)
    implicit none
    type(complex_num) :: opfunc_div
    type(complex_num),intent(in) :: c1,c2
    real::d
    d= 1/(c2%real_part**2-p*c2%image_part**2)
    opfunc_div%real_part = d *(c1%real_part * c2%real_part -p*c1%image_part * c2%image_part)
    opfunc_div%image_part = d * (c2%real_part*c1%image_part-c1%real_part*c2%image_part)
    return
  end function opfunc_div

end module complex_ope

program complex_operation
  use complex_ope
  implicit none
  type(complex_num) :: c1,c2
  c1%real_part = 9
  c1%image_part = 10
  c2%real_part = 11
  c2%image_part = 12
  print *,"generalized complex nuber c1 is (9,10)"
  print *,"generalized complex number c2 is (11,12)"
  print *,"p = 1"
  print *
  print*,"addition:"
  write(6,*) c1+c2
  print*,"substractiony:"
  write(6,*) c1-c2
  print*,"multiplication:"
  write(6,*) c1*c2
  print*,"division:"
  write(6,*) c1/c2

  stop
end program complex_operation

