(de espia (s) (prin3 s) (terpri) s)
(de spy (s w) (prin3 w) (prin3 '->) (prin3 s) (terpri) s)

(de EXISTE (A L)
  (COND
    ((NULL (spy L 'L)) NIL)

    ((NOT (listp (FIRST L ))) 
      (OR (EQUAL A (FIRST L)) (EXISTE A (REST L))))

    (T (OR (EXISTE A (FIRST L)) (EXISTE A (REST L))))))