! qcomplex.f90
! a class for 32-byte (quadruple precision) complex numbers
! this file is part of LoopTools
! last modified 18 Oct 01 th


module qcomplex

implicit none

type complex32
  sequence
  real*16 re, im
end type complex32

interface assignment (=)
  module procedure d2q, q2d, q2q, r2q, i2q, q2r
end interface

interface operator (+)
  module procedure q_plus, qq_add, &
                   qd_add, dq_add, &
                   qr_add, rq_add, &
                   qi_add, iq_add
end interface

interface operator (-)
  module procedure q_minus, qq_sub, &
                   qd_sub, dq_sub, &
                   qr_sub, rq_sub, &
                   qi_sub, iq_sub
end interface

interface operator (*)
  module procedure qq_mul, &
                   qd_mul, dq_mul, &
                   qr_mul, rq_mul, &
                   qi_mul, iq_mul
end interface

interface operator (/)
  module procedure qq_div, &
                   qd_div, dq_div, &
                   qr_div, rq_div, &
                   qi_div, iq_div
end interface

interface operator (**)
  module procedure qq_pow, qr_pow, qi_pow
end interface

interface operator (.eq.)
  module procedure qq_eq, &
                   qd_eq, dq_eq, &
                   qr_eq, rq_eq, &
                   qi_eq, iq_eq
end interface

interface operator (.ne.)
  module procedure qq_ne, &
                   qd_ne, dq_ne, &
                   qr_ne, rq_ne, &
                   qi_ne, iq_ne
end interface

interface log
  module procedure q_log
end interface

interface exp
  module procedure q_exp
end interface

interface sqrt
  module procedure q_sqrt
end interface

interface sin
  module procedure q_sin
end interface

interface cos
  module procedure q_cos
end interface

interface dble
  module procedure q_real
end interface

interface dimag
  module procedure q_imag
end interface

interface dcmplx
  module procedure q_cmplx, r_cmplx, rr_cmplx
end interface

interface dconjg
  module procedure q_conjg
end interface

interface abs
  module procedure q_abs
end interface

contains

subroutine d2q(q, d)
  type(complex32), intent(out) :: q
  double complex, intent(in) :: d
  q%re = qext(dble(d))
  q%im = qext(dimag(d))
end subroutine d2q

subroutine q2d(d, q)
  double complex, intent(out) :: d
  type(complex32), intent(in) :: q
  d = dcmplx(dble(q%re), dble(q%im))
end subroutine q2d

subroutine q2q(q1, q2)
  type(complex32), intent(out) :: q1
  type(complex32), intent(in) :: q2
  q1%re = q2%re
  q1%im = q2%im
end subroutine q2q

subroutine r2q(q, r)
  type(complex32), intent(out) :: q
  real*16, intent(in) :: r
  q%re = r
  q%im = 0
end subroutine r2q

subroutine i2q(q, i)
  type(complex32), intent(out) :: q
  integer, intent(in) :: i
  q%re = i
  q%im = 0
end subroutine i2q

subroutine q2r(r, q)
  real*16, intent(out) :: r
  type(complex32), intent(in) :: q
  r = q%re
end subroutine q2r

function q_plus(a)
  type(complex32), intent(in) :: a
  type(complex32) :: q_plus
  q_plus%re = a%re
  q_plus%im = a%im
end function q_plus

function qq_add(a, b)
  type(complex32), intent(in) :: a, b
  type(complex32) :: qq_add
  qq_add%re = a%re + b%re
  qq_add%im = a%im + b%im
end function qq_add

function qd_add(a, b)
  type(complex32), intent(in) :: a
  double complex, intent(in) :: b
  type(complex32) :: qd_add
  qd_add%re = a%re + qext(b)
  qd_add%im = a%im + qext(dimag(b))
end function qd_add

function dq_add(a, b)
  double complex, intent(in) :: a
  type(complex32), intent(in) :: b
  type(complex32) :: dq_add
  dq_add%re = qext(a) + b%re
  dq_add%im = qext(dimag(a)) + b%im
end function dq_add

function qr_add(a, b)
  type(complex32), intent(in) :: a
  real*16, intent(in) :: b
  type(complex32) :: qr_add
  qr_add%re = a%re + b
  qr_add%im = a%im
end function qr_add

function rq_add(a, b)
  real*16, intent(in) :: a
  type(complex32), intent(in) :: b
  type(complex32) :: rq_add
  rq_add%re = a + b%re
  rq_add%im = b%im
end function rq_add

function qi_add(a, b)
  type(complex32), intent(in) :: a
  integer, intent(in) :: b
  type(complex32) :: qi_add
  qi_add%re = a%re + b
  qi_add%im = a%im
end function qi_add

function iq_add(a, b)
  integer, intent(in) :: a
  type(complex32), intent(in) :: b
  type(complex32) :: iq_add
  iq_add%re = a + b%re
  iq_add%im = b%im
end function iq_add

function q_minus(a)
  type(complex32), intent(in) :: a
  type(complex32) :: q_minus
  q_minus%re = -a%re
  q_minus%im = -a%im
end function q_minus

function qq_sub(a, b)
  type(complex32), intent(in) :: a, b
  type(complex32) :: qq_sub
  qq_sub%re = a%re - b%re
  qq_sub%im = a%im - b%im
end function qq_sub

function qd_sub(a, b)
  type(complex32), intent(in) :: a
  double complex, intent(in) :: b
  type(complex32) :: qd_sub
  qd_sub%re = a%re - qext(b)
  qd_sub%im = a%im - qext(dimag(b))
end function qd_sub

function dq_sub(a, b)
  double complex, intent(in) :: a
  type(complex32), intent(in) :: b
  type(complex32) :: dq_sub
  dq_sub%re = qext(a) - b%re
  dq_sub%im = qext(dimag(a)) - b%im
end function dq_sub

function qr_sub(a, b)
  type(complex32), intent(in) :: a
  real*16, intent(in) :: b
  type(complex32) :: qr_sub
  qr_sub%re = a%re - b
  qr_sub%im = a%im
end function qr_sub

function rq_sub(a, b)
  real*16, intent(in) :: a
  type(complex32), intent(in) :: b
  type(complex32) :: rq_sub
  rq_sub%re = a - b%re
  rq_sub%im = -b%im
end function rq_sub

function qi_sub(a, b)
  type(complex32), intent(in) :: a
  integer, intent(in) :: b
  type(complex32) :: qi_sub
  qi_sub%re = a%re - b
  qi_sub%im = a%im
end function qi_sub

function iq_sub(a, b)
  integer, intent(in) :: a
  type(complex32), intent(in) :: b
  type(complex32) :: iq_sub
  iq_sub%re = a - b%re
  iq_sub%im = -b%im
end function iq_sub

function qq_mul(a, b)
  type(complex32), intent(in) :: a, b
  type(complex32) :: qq_mul
  qq_mul%re = a%re*b%re - a%im*b%im
  qq_mul%im = a%re*b%im + a%im*b%re
end function qq_mul

function qd_mul(a, b)
  type(complex32), intent(in) :: a
  double complex, intent(in) :: b
  type(complex32) :: qd_mul
  qd_mul%re = a%re*qext(b) - a%im*qext(dimag(b))
  qd_mul%im = a%re*qext(dimag(b)) + a%im*qext(b)
end function qd_mul

function dq_mul(a, b)
  double complex, intent(in) :: a
  type(complex32), intent(in) :: b
  type(complex32) :: dq_mul
  dq_mul%re = qext(a)*b%re - qext(dimag(a))*b%im
  dq_mul%im = qext(a)*b%im + qext(dimag(a))*b%re
end function dq_mul

function qr_mul(a, b)
  type(complex32), intent(in) :: a
  real*16, intent(in) :: b
  type(complex32) :: qr_mul
  qr_mul%re = a%re*b
  qr_mul%im = a%im*b
end function qr_mul

function rq_mul(a, b)
  real*16, intent(in) :: a
  type(complex32), intent(in) :: b
  type(complex32) :: rq_mul
  rq_mul%re = a*b%re
  rq_mul%im = a*b%im
end function rq_mul

function qi_mul(a, b)
  type(complex32), intent(in) :: a
  integer, intent(in) :: b
  type(complex32) :: qi_mul
  qi_mul%re = a%re*b
  qi_mul%im = a%im*b
end function qi_mul

function iq_mul(a, b)
  integer, intent(in) :: a
  type(complex32), intent(in) :: b
  type(complex32) :: iq_mul
  iq_mul%re = a*b%re
  iq_mul%im = a*b%im
end function iq_mul

function qq_div(a, b)
  type(complex32), intent(in) :: a, b
  type(complex32) :: qq_div
  real*16 :: ratio, den
  if(abs(b%re) .le. abs(b%im)) then
    ratio = b%re/b%im
    den = b%im*(1 + ratio**2)
    qq_div%re = (a%re*ratio + a%im)/den
    qq_div%im = (a%im*ratio - a%re)/den
  else
    ratio = b%im/b%re
    den = b%re*(1 + ratio**2)
    qq_div%re = (a%re + a%im*ratio)/den
    qq_div%im = (a%im - a%re*ratio)/den
  endif
end function qq_div

function qd_div(a, b)
  type(complex32), intent(in) :: a
  double complex, intent(in) :: b
  type(complex32) :: qd_div
  real*16 :: ratio, den
  if(abs(qext(b)) .le. abs(qext(dimag(b)))) then
    ratio = qext(b)/qext(dimag(b))
    den = qext(dimag(b))*(1 + ratio**2)
    qd_div%re = (a%re*ratio + a%im)/den
    qd_div%im = (a%im*ratio - a%re)/den
  else
    ratio = qext(dimag(b))/qext(b)
    den = qext(b)*(1 + ratio**2)
    qd_div%re = (a%re + a%im*ratio)/den
    qd_div%im = (a%im - a%re*ratio)/den
  endif
end function qd_div

function dq_div(a, b)
  double complex, intent(in) :: a
  type(complex32), intent(in) :: b
  type(complex32) :: dq_div
  real*16 :: ratio, den
  if(abs(b%re) .le. abs(b%im)) then
    ratio = b%re/b%im
    den = b%im*(1 + ratio**2)
    dq_div%re = (qext(a)*ratio + qext(dimag(a)))/den
    dq_div%im = (qext(dimag(a))*ratio - qext(a))/den
  else
    ratio = b%im/b%re
    den = b%re*(1 + ratio**2)
    dq_div%re = (qext(a) + qext(dimag(a))*ratio)/den
    dq_div%im = (qext(dimag(a)) - qext(a)*ratio)/den
  endif
end function dq_div

function qr_div(a, b)
  type(complex32), intent(in) :: a
  real*16, intent(in) :: b
  type(complex32) :: qr_div
  qr_div%re = a%re/b
  qr_div%im = a%im/b
end function qr_div

function rq_div(a, b)
  real*16, intent(in) :: a
  type(complex32), intent(in) :: b
  type(complex32) :: rq_div
  real*16 :: ratio, den
  if(abs(b%re) .le. abs(b%im)) then
    ratio = b%re/b%im
    den = b%im*(1 + ratio**2)
    rq_div%re = a*ratio/den
    rq_div%im = -a/den
  else
    ratio = b%im/b%re
    den = b%re*(1 + ratio**2)
    rq_div%re = a/den
    rq_div%im = -a*ratio/den
  endif
end function rq_div

function qi_div(a, b)
  type(complex32), intent(in) :: a
  integer, intent(in) :: b
  type(complex32) :: qi_div
  qi_div%re = a%re/b
  qi_div%im = a%im/b
end function qi_div

function iq_div(a, b)
  integer, intent(in) :: a
  type(complex32), intent(in) :: b
  type(complex32) :: iq_div
  real*16 :: ratio, den
  if(abs(b%re) .le. abs(b%im)) then
    ratio = b%re/b%im
    den = b%im*(1 + ratio**2)
    iq_div%re = a*ratio/den
    iq_div%im = -a/den
  else
    ratio = b%im/b%re
    den = b%re*(1 + ratio**2)
    iq_div%re = a/den
    iq_div%im = -a*ratio/den
  endif
end function iq_div

function qq_pow(a, b)
  type(complex32), intent(in) :: a, b
  type(complex32) :: qq_pow
  real*16 :: logr, logi, x, y
  logr = .5q0*log(a%re**2 + a%im**2)
  logi = atan2(a%im, a%re)
  x = exp(logr*b%re - logi*b%im)
  y = logr*b%im + logi*b%re
  qq_pow%re = x*cos(y)
  qq_pow%im = x*sin(y)
end function qq_pow

function qr_pow(a, b)
  type(complex32), intent(in) :: a
  real*16, intent(in) :: b
  type(complex32) :: qr_pow
  real*16 :: x, y
  x = exp(.5q0*log(a%re**2 + a%im**2)*b)
  y = atan2(a%im, a%re)*b
  qr_pow%re = x*cos(y)
  qr_pow%im = x*sin(y)
end function qr_pow

function qi_pow(a, b)
  type(complex32), intent(in) :: a
  integer, intent(in) :: b
  type(complex32) :: qi_pow
  integer :: n
  type(complex32) :: x
  qi_pow%re = 1
  qi_pow%im = 0
  if(b .eq. 0) return
  n = b
  if(n .lt. 0) then
    n = -n
    x = 1/a
  else
    x = a
  endif
  do
    if(btest(n, 0)) qi_pow = qi_pow*x
    n = n/2
    if(n .eq. 0) exit
    x = x*x
  enddo
end function qi_pow

function qq_eq(a, b)
  type(complex32), intent(in) :: a, b
  logical :: qq_eq
  qq_eq = a%re .eq. b%re .and. a%im .eq. b%im
end function qq_eq

function qd_eq(a, b)
  type(complex32), intent(in) :: a
  double complex, intent(in) :: b
  logical :: qd_eq
  qd_eq = dbleq(a%re) .eq. dble(b) .and. &
    dbleq(a%im) .eq. qext(dimag(b))
end function qd_eq

function dq_eq(a, b)
  double complex, intent(in) :: a
  type(complex32), intent(in) :: b
  logical :: dq_eq
  dq_eq = dble(a) .eq. dbleq(b%re) .and. &
    qext(dimag(a)) .eq. dbleq(b%im)
end function dq_eq

function qr_eq(a, b)
  type(complex32), intent(in) :: a
  real*16, intent(in) :: b
  logical :: qr_eq
  qr_eq = a%re .eq. b .and. a%im .eq. 0
end function qr_eq

function rq_eq(a, b)
  real*16, intent(in) :: a
  type(complex32), intent(in) :: b
  logical :: rq_eq
  rq_eq = b%re .eq. a .and. b%im .eq. 0
end function rq_eq

function qi_eq(a, b)
  type(complex32), intent(in) :: a
  integer, intent(in) :: b
  logical :: qi_eq
  qi_eq = a%re .eq. b .and. a%im .eq. 0
end function qi_eq

function iq_eq(a, b)
  integer, intent(in) :: a
  type(complex32), intent(in) :: b
  logical :: iq_eq
  iq_eq = b%re .eq. a .and. b%im .eq. 0
end function iq_eq

function qq_ne(a, b)
  type(complex32), intent(in) :: a, b
  logical :: qq_ne
  qq_ne = a%re .ne. b%re .or. a%im .ne. b%im
end function qq_ne

function qd_ne(a, b)
  type(complex32), intent(in) :: a
  double complex, intent(in) :: b
  logical :: qd_ne
  qd_ne = dbleq(a%re) .ne. dble(b) .or. &
    dbleq(a%im) .ne. dble(dimag(b))
end function qd_ne

function dq_ne(a, b)
  double complex, intent(in) :: a
  type(complex32), intent(in) :: b
  logical :: dq_ne
  dq_ne = dble(a) .ne. dbleq(b%re) .or. &
    dble(dimag(a)) .ne. dbleq(b%im)
end function dq_ne

function qr_ne(a, b)
  type(complex32), intent(in) :: a
  real*16, intent(in) :: b
  logical :: qr_ne
  qr_ne = a%re .ne. b .or. a%im .ne. 0
end function qr_ne

function rq_ne(a, b)
  real*16, intent(in) :: a
  type(complex32), intent(in) :: b
  logical :: rq_ne
  rq_ne = b%re .ne. a .or. b%im .ne. 0
end function rq_ne

function qi_ne(a, b)
  type(complex32), intent(in) :: a
  integer, intent(in) :: b
  logical :: qi_ne
  qi_ne = a%re .ne. b .or. a%im .ne. 0
end function qi_ne

function iq_ne(a, b)
  integer, intent(in) :: a
  type(complex32), intent(in) :: b
  logical :: iq_ne
  iq_ne = b%re .ne. a .or. b%im .ne. 0
end function iq_ne

function q_log(a)
  type(complex32), intent(in) :: a
  type(complex32) :: q_log
  q_log%re = .5q0*log(a%re**2 + a%im**2)
  q_log%im = atan2(a%im, a%re)
end function q_log

function q_exp(a)
  type(complex32), intent(in) :: a
  type(complex32) :: q_exp
  real*16 :: expr
  expr = exp(a%re)
  q_exp%re = expr*cos(a%im)
  q_exp%im = expr*sin(a%im)
end function q_exp

function q_sqrt(a)
  type(complex32), intent(in) :: a
  type(complex32) :: q_sqrt
  real*16 :: mag
  mag = abs(a)
  if(mag .eq. 0) then
    q_sqrt%re = 0
    q_sqrt%im = 0
  else if(a%re .gt. 0) then
    q_sqrt%re = sqrt(.5q0*(mag + a%re))
    q_sqrt%im = .5q0*a%im/q_sqrt%re
  else
    q_sqrt%im = sign(sqrt(.5q0*(mag - a%re)), a%im)
    q_sqrt%re = .5q0*a%im/q_sqrt%im
  endif
end function q_sqrt

function q_sin(a)
  type(complex32), intent(in) :: a
  type(complex32) :: q_sin
  q_sin%re = sin(a%re)*cosh(a%im)
  q_sin%im = cos(a%re)*sinh(a%im)
end function q_sin

function q_cos(a)
  type(complex32), intent(in) :: a
  type(complex32) :: q_cos
  q_cos%re = cos(a%re)*cosh(a%im)
  q_cos%im = -sin(a%re)*sinh(a%im)
end function q_cos

function q_real(a)
  type(complex32), intent(in) :: a
  real*16 :: q_real
  q_real = a%re
end function q_real

function q_imag(a)
  type(complex32), intent(in) :: a
  real*16 :: q_imag
  q_imag = a%im
end function q_imag

function q_cmplx(a)
  type(complex32), intent(in) :: a
  type(complex32) :: q_cmplx
  q_cmplx%re = a%re
  q_cmplx%im = a%im
end function q_cmplx

function r_cmplx(a)
  real*16, intent(in) :: a
  type(complex32) :: r_cmplx
  r_cmplx%re = a
  r_cmplx%im = 0
end function r_cmplx

function rr_cmplx(a,b)
  real*16, intent(in) :: a, b
  type(complex32) :: rr_cmplx
  rr_cmplx%re = a
  rr_cmplx%im = b
end function rr_cmplx

function q_conjg(a)
  type(complex32), intent(in) :: a
  type(complex32) :: q_conjg
  q_conjg%re = a%re
  q_conjg%im = -a%im
end function q_conjg

function q_abs(a)
  type(complex32), intent(in) :: a
  real*16 :: q_abs
  q_abs = sqrt(a%re**2 + a%im**2)
end function q_abs

end module qcomplex

