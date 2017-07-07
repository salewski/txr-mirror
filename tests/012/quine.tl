(load "../common")

(test
  ((lambda (lambda)
     ^(,lambda ',lambda))
   '(lambda (lambda) ^(,lambda ',lambda)))
  ((lambda (lambda)
     ^(,lambda ',lambda))
   '(lambda (lambda) ^(,lambda ',lambda))))

(test
  ((lambda (lambda)
     ^((lambda (lambda) ,lambda) ',lambda))
   '^((lambda (lambda) ,lambda) ',lambda))
  ((lambda (lambda)
     ^((lambda (lambda) ,lambda) ',lambda))
   '^((lambda (lambda) ,lambda) ',lambda)))
