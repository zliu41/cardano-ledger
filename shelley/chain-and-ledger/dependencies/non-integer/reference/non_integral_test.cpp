#include <chrono>
#include <iostream>
#include <sstream>
#include <string>

#include <gmpxx.h>

#include "non_integral.hpp"

const std::string print_fixedp(
  const mpz_class &n,
  const mpz_class &precision,
  const size_t width)
{
  mpz_class temp_r, temp_q;
  // use truncate rounding here for consistency
  mpz_tdiv_qr(temp_q.get_mpz_t(),
              temp_r.get_mpz_t(),
              n.get_mpz_t(),
              precision.get_mpz_t());

  std::stringstream s;
  s << temp_q << ".";
  s.fill('0');
  s.width(width);
  s << temp_r;

  return s.str();
}

int main()
{

  mpz_class ten("10");
  mpz_class precision;
  mpz_pow_ui(precision.get_mpz_t(), ten.get_mpz_t(), 34);

  mpz_class epsilon;
  mpz_pow_ui(epsilon.get_mpz_t(), ten.get_mpz_t(), 34 - 24);

  mpz_class resolution;
  mpz_pow_ui(resolution.get_mpz_t(), ten.get_mpz_t(), 17);

  initialize(precision.get_mpz_t(), epsilon.get_mpz_t());

  std::chrono::duration<double> total =
    std::chrono::duration<double>::zero();
  std::chrono::duration<double> total_cmp =
    std::chrono::duration<double>::zero();
  std::chrono::duration<double> total_cmp_linear =
    std::chrono::duration<double>::zero();
  std::chrono::duration<double> maximal =
    std::chrono::duration<double>::zero();
  std::chrono::duration<double> maximal_cmp =
    std::chrono::duration<double>::zero();
  std::chrono::duration<double> maximal_cmp_linear =
    std::chrono::duration<double>::zero();

  // format is "sigma p"
  size_t n = 0;

  mpz_class f;
  mpz_class one("1");
  one *= precision;
  f = one / ten;

  mpz_class e;
  ref_exp(e.get_mpz_t(), one.get_mpz_t());

  for (std::string s; std::getline(std::cin, s); )
    {
      const size_t split = s.find(' ');
      const size_t split0 = s.find(' ', split + 1);
      if(split != std::string::npos && split0 != std::string::npos)
        {
          mpz_class x(s.substr(0, split));
          mpz_class a(s.substr(split, split0 - split));
          mpz_class b(s.substr(split0, std::string::npos));

          mpz_class exp_x;
          mpz_class ln_a;
          mpz_class threshold_b;
          mpz_class approx;
          mpz_class error;

          std::chrono::duration<double> diff;
          mp_exp_cmp_result res;

          // calculate exp' x
          {
            mpz_class base = one - f;
            ref_exp(exp_x.get_mpz_t(), x.get_mpz_t());
            std::cout << print_fixedp(exp_x, precision, 34);
          }

          // calculate ln' a, print -ln' a
          {
            ref_ln(ln_a.get_mpz_t(), a.get_mpz_t());
            std::cout << " " << print_fixedp(-ln_a, precision, 34);
          }

          // calculate (1 - f) *** b
          {
            mpz_class c;
            c = one - f;
            auto before = std::chrono::high_resolution_clock::now();
            ref_pow(threshold_b.get_mpz_t(), c.get_mpz_t(), b.get_mpz_t());
            auto after = std::chrono::high_resolution_clock::now();
            diff = after - before;
            total += diff;
            if(maximal < diff)
              maximal = diff;

            std::cout << " " << print_fixedp(one - threshold_b, precision, 34);
          }

          // as above, but use linear approximation first, then go for Taylor
          // series
          {
            mpz_class temp;
            mpz_class c;
            c = one - f;
            ref_ln(temp.get_mpz_t(), c.get_mpz_t());
            mpz_class alpha = b * temp;
            scale(alpha.get_mpz_t());
            alpha = -alpha;     // negate after scale

            mpz_class q_ = one - a;
            mpz_class q;
            ref_div(q.get_mpz_t(), one.get_mpz_t(), q_.get_mpz_t());

            auto before = std::chrono::high_resolution_clock::now();


            // 3 is used as bound, might need to be adapted for other use cases
            res =
              ref_exp_cmp(approx.get_mpz_t(), 1000, alpha.get_mpz_t(), 3, q.get_mpz_t());
            auto after = std::chrono::high_resolution_clock::now();

            diff = after - before;
            total_cmp += diff;
            if(maximal_cmp < diff)
              maximal_cmp = diff;

            // we compare 1/(1-p) < e^-(1-(1-f)^sigma)
            if(a < (one - threshold_b) && res.estimate != LT)
              {
                std::cout << "wrong result should be leader "
                          << print_fixedp(temp, precision, 34)
                          << " should be more like "
                          << print_fixedp(one - threshold_b, precision, 34)
                          << std::endl;
              }

            if(!(a < (one - threshold_b)) && res.estimate != GT)
              {
                std::cout << "wrong result should not be leader "
                          << print_fixedp(temp, precision, 34)
                          << " should be more like "
                          << print_fixedp(one - threshold_b, precision, 34)
                          << std::endl;
              }

            std::cout << " " << print_fixedp(approx, precision, 34)
                      << " " << (res.estimate == LT ? "LT" : "GT")
                      << " " << res.iterations << std::endl;
          }

          // calculate arctan(x)
          {
            // distance is
            // -(-f + 1)^(log(-f/log(-f + 1))/log(-f + 1)) - f*log(-f/log(-f + 1))/log(-f + 1) + 1

            mpz_class oneMf = one - f;
            mpz_class logfp1;
            ref_ln(logfp1.get_mpz_t(), oneMf.get_mpz_t()); // log(1 - f)

            // -f/log(1 - f)
            mpz_class tmp;
            ref_div(tmp.get_mpz_t(), f.get_mpz_t(), logfp1.get_mpz_t());
            tmp = -tmp;

            // log(-f/log(-f + 1))/log(-f + 1)
            mpz_class tmp2;
            ref_ln(tmp2.get_mpz_t(), tmp.get_mpz_t());
            mpz_class logTerm;
            ref_div(logTerm.get_mpz_t(), tmp2.get_mpz_t(), logfp1.get_mpz_t());

            mpz_class fLogTerm = f*logTerm;
            scale(fLogTerm.get_mpz_t());

            mpz_class powLogTerm;
            ref_pow(powLogTerm.get_mpz_t(), oneMf.get_mpz_t(), logTerm.get_mpz_t());

            mpz_class distance = one - fLogTerm - powLogTerm;

            mpz_class angle;
            arctan(angle.get_mpz_t(), f.get_mpz_t());

            mpz_class s;
            sine(s.get_mpz_t(), angle.get_mpz_t());

            // calculate y-axis displacement
            mpz_class y0, y, r;
            y0 = distance * f;
            scale(y0.get_mpz_t());
            ref_div(y.get_mpz_t(), y0.get_mpz_t(), s.get_mpz_t());

            mpz_class temp;
            mpz_class c;
            c = one - f;
            ref_ln(temp.get_mpz_t(), c.get_mpz_t());
            mpz_class alpha = b * temp;
            scale(alpha.get_mpz_t());
            alpha = -alpha;     // negate after scale

            mpz_class q_ = one - a;
            mpz_class q;
            ref_div(q.get_mpz_t(), one.get_mpz_t(), q_.get_mpz_t());

            mpz_class cmp;

            auto before = std::chrono::high_resolution_clock::now();

            // first check for a < f * b as linear approximation, if true ->
            // leader
            cmp = f * b;
            scale(cmp.get_mpz_t());

            if (a > cmp + y)
              {
                //std::cout << "YAY";
                res.iterations = 0;
                res.estimate = GT;
              }
            else if (a < cmp)
              {
                //std::cout << "YAY";
                res.iterations = 0;
                res.estimate = LT;
              }
            else
              {
                // // 3 is used as bound, might need to be adapted for other use cases
                //std::cout << "NAY";
                res =
                  ref_exp_cmp(approx.get_mpz_t(), 1000, alpha.get_mpz_t(), 3, q.get_mpz_t());
              }
            auto after = std::chrono::high_resolution_clock::now();

            diff = after - before;
            total_cmp_linear += diff;
            if(maximal_cmp_linear < diff)
              maximal_cmp_linear = diff;

            // we compare 1/(1-p) < e^-(1-(1-f)^sigma)
            if(a < (one - threshold_b) && res.estimate != LT)
              {
                std::cout << "linear wrong result should be leader "
                          << print_fixedp(temp, precision, 34)
                          << " should be more like "
                          << print_fixedp(one - threshold_b, precision, 34)
                          << std::endl;
              }

            if(!(a < (one - threshold_b)) && res.estimate != GT)
              {
                std::cout << "linear wrong result should not be leader "
                          << print_fixedp(temp, precision, 34)
                          << " should be more like "
                          << print_fixedp(one - threshold_b, precision, 34)
                          << std::endl;
              }

            std::cout << " " << print_fixedp(approx, precision, 34)
                      << " " << (res.estimate == LT ? "LT" : "GT")
                      << " " << res.iterations << std::endl;
          }


          n++;
        }
      else
        std::cout << "format error" << std::endl;
    }

  std::cerr << "(1 - (1 - f))^b avg: " << (total.count() / n)
            << " maximal time: " << maximal.count()
            << std::endl;
  std::cerr << "Taylor error estimation avg: " << (total_cmp.count() / n)
            << " maximal time: " << maximal_cmp.count()
            << std::endl;
  std::cerr << "Taylor error estimation with linear approx avg: "
            << (total_cmp_linear.count() / n)
            << " maximal time: " << maximal_cmp_linear.count()
            << std::endl;

  cleanup();
  return 0;
}
