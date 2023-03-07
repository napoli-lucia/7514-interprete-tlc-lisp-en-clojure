(ns interprete-01.core
  (:gen-class))

(require '[clojure.string :refer [blank? ends-with? lower-case]] '[clojure.java.io :refer [reader]])


(defn spy
  ([x] (do (prn x) (prn) x))
  ([msg x] (do (print msg) (print ": ") (prn x) (prn) x))
  )

; Funciones principales
(declare repl)
(declare evaluar)
(declare aplicar)

; Funciones secundarias de evaluar
(declare evaluar-de)
(declare evaluar-if)
(declare evaluar-or)
(declare evaluar-cond)
(declare evaluar-eval)
(declare evaluar-exit)
(declare evaluar-load)
(declare evaluar-setq)
(declare evaluar-quote)
(declare evaluar-lambda)
(declare evaluar-escalar)

; Funciones secundarias de aplicar
(declare aplicar-lambda)
(declare aplicar-funcion-primitiva)

; Funciones primitivas
(declare fnc-ge)
(declare fnc-gt)
(declare fnc-lt)
(declare fnc-add)
(declare fnc-env)
(declare fnc-not)
(declare fnc-sub)
(declare fnc-cons)
(declare fnc-list)
(declare fnc-null)
(declare fnc-read)
(declare fnc-rest)
(declare fnc-equal)
(declare fnc-first)
(declare fnc-listp)
(declare fnc-prin3)
(declare fnc-append)
(declare fnc-length)
(declare fnc-terpri)
(declare fnc-reverse)

; Funciones auxiliares
(declare buscar)
(declare error?)
(declare igual?)
(declare imprimir)
(declare cargar-arch)
(declare revisar-fnc)
(declare revisar-lae)
(declare actualizar-amb)
(declare controlar-aridad)
(declare aplicar-lambda-simple)
(declare aplicar-lambda-multiple)
(declare evaluar-clausulas-en-cond)
(declare evaluar-secuencia-en-cond)

(defn -main
  [& args]
  (repl)
  )

; REPL (read–eval–print loop).
; Aridad 0: Muestra mensaje de bienvenida y se llama recursivamente con el ambiente inicial.
; Aridad 1: Muestra >>> y lee una expresion y la evalua. El resultado es una lista con un valor y un ambiente.
; Si la 2da. posicion del resultado es nil, devuelve true (caso base de la recursividad).
; Si no, imprime la 1ra. pos. del resultado y se llama recursivamente con la 2da. pos. del resultado.
(defn repl
  "Inicia el REPL de TLC-LISP."
  ([]
   (println "Interprete de TLC-LISP en Clojure")
   (println "Trabajo Practico de 75.14/95.48 - Lenguajes Formales 2022")
   (println "Inspirado en:")
   (println "  TLC-LISP Version 1.51 for the IBM Personal Computer")
   (println "  Copyright (c) 1982, 1983, 1984, 1985 The Lisp Company") (flush)
   (repl '(add add append append cond cond cons cons de de env env equal equal
               eval eval exit exit first first ge ge gt gt if if lambda lambda
               length length list list listp listp load load lt lt nil nil
               not not null null or or prin3 prin3 quote quote read read
               rest rest reverse reverse setq setq sub sub t t terpri terpri
               + add - sub)))
  ([amb]
   (print ">>> ") (flush)
   (try
     (let [res (evaluar (read) amb nil)]  ; READ, EVAL
       (if (nil? (second res))
         true
         (do (imprimir (first res))     ; PRINT
             (repl (second res)))))     ; LOOP
     (catch Exception e
       (println) (print "*error* ")
       (println (get (Throwable->map e) :cause))
       (repl amb)))))


(defn evaluar
  "Evalua una expresion 'expre' en los ambientes global y local. Devuelve un lista con un valor resultante y un ambiente."
  [expre amb-global amb-local]
  (if (or (igual? expre nil)
          (and (seq? expre)
               (or (empty? expre) (error? expre)))) ; si 'expre' es nil, () o error, devolverla intacta
    (list expre amb-global)                       ; de lo contrario, evaluarla
    (cond
      (not (seq? expre))             (evaluar-escalar expre amb-global amb-local)

      (igual? (first expre) 'cond)   (evaluar-cond expre amb-global amb-local)
      (igual? (first expre) 'de)     (evaluar-de expre amb-global)

      (igual? (first expre) 'eval)     (evaluar-eval expre amb-global amb-local)
      (igual? (first expre) 'load)     (evaluar-load expre amb-global amb-local)
      (igual? (first expre) 'exit)     (evaluar-exit expre amb-global amb-local)
      (igual? (first expre) 'lambda)   (evaluar-lambda expre amb-global amb-local)
      (igual? (first expre) 'quote)    (evaluar-quote expre amb-global amb-local)

      ;(igual? (first expre) 'escalar)  (evaluar-escalar expre amb-global amb-local)
      (igual? (first expre) 'if)  (evaluar-if expre amb-global amb-local)
      (igual? (first expre) 'or)  (evaluar-or expre amb-global amb-local)
      (igual? (first expre) 'setq)  (evaluar-setq expre amb-global amb-local)
      ;
      ;
      ;
      ; Si la expresion no es la aplicacion de una funcion (es una forma especial, una macro...) debe ser evaluada aqui
      ; por una funcion de Clojure especifica debido a que puede ser necesario evitar la evaluacion de los argumentos
      ;
      ;
      ;

      :else (let [res-eval-1 (evaluar (first expre) amb-global amb-local),
                  res-eval-2 (reduce (fn [x y] (let [res-eval-3 (evaluar y (first x) amb-local)] (cons (second res-eval-3) (concat (next x) (list (first res-eval-3)))))) (cons (list (second res-eval-1)) (next expre)))]
              (aplicar (first res-eval-1) (next res-eval-2) (first res-eval-2) amb-local)))))


; Evalua una macro COND. Siempre devuelve una lista con un resultado y un ambiente.
(defn evaluar-cond [expre amb-global amb-local]
  "Evalua una forma 'cond' en TLC-LISP."
  (evaluar-clausulas-en-cond (next expre) amb-global amb-local))


(defn evaluar-clausulas-en-cond [expre amb-global amb-local]
  "Une 'evaluar-cond' con 'evaluar-secuencia-en-cond'."
  (if (nil? expre)
    (list nil amb-global)
    (let [res-eval (evaluar (ffirst expre) amb-global amb-local)]
      (cond
        (error? (first res-eval)) res-eval
        (not (igual? (first res-eval) nil)) (evaluar-secuencia-en-cond (nfirst expre) (second res-eval) amb-local)
        :else (recur (next expre) (second res-eval) amb-local)))))


; Evalua (con evaluar) secuencialmente las sublistas de una lista y devuelve el valor de la ultima evaluacion.
; Si alguna evaluacion devuelve un error, sera la ultima que se evalue.
(defn evaluar-secuencia-en-cond [lis amb-global amb-local]
  (if (nil? (next lis))
    (evaluar (first lis) amb-global amb-local)
    (let [res-eval (evaluar (first lis) amb-global amb-local)]
      (if (error? (first res-eval))
        res-eval
        (recur (next lis) (second res-eval) amb-local)))))


(defn evaluar-eval
  "Evalua una forma 'eval' en TLC-LISP."
  [expre amb-global amb-local]
  (let [ari (controlar-aridad (next expre) 1)]
    (cond
      (seq? ari) ari
      (and (seq? (second expre)) (igual? (first (second expre)) 'quote)) (evaluar (second (second expre)) amb-global amb-local)
      :else (evaluar (second expre) amb-global amb-local))))


(defn evaluar-exit
  "Sale del interprete de TLC-LISP."
  [expre amb-global _]
  (cond
    (< (count (next expre)) 1) (list nil nil)
    :else (list (list '*error* 'too-many-args) amb-global)))


(defn evaluar-lambda
  "Evalua una forma 'lambda' en TLC-LISP."
  [expre amb-global _]
  (cond
    (< (count (next expre)) 1) (list (list '*error* 'list 'expected nil) amb-global)
    (and (not (igual? (second expre) nil)) (not (seq? (second expre))))
    (list (list '*error* 'list 'expected (second expre)) amb-global)
    :else (list expre amb-global)))


(defn evaluar-load
  "Evalua una forma 'load' en TLC-LISP. Carga en el ambiente un archivo 'expre' con código en TLC-LISP."
  [expre amb-global amb-local]
  (cond
    (< (count (next expre)) 1) (list (list '*error* 'too-few-args) amb-global)
    (> (count (next expre)) 1) (list (list '*error* 'not-implemented) amb-global)
    :else (list \space (cargar-arch amb-global amb-local (second expre)))))


(defn cargar-arch
  ([amb-global amb-local arch]
   (let [nomb (first (evaluar arch amb-global amb-local))]
     (if (error? nomb)
       (do (imprimir nomb) amb-global)
       (let [nm (clojure.string/lower-case (str nomb)),
             nom (if (and (> (count nm) 4) (clojure.string/ends-with? nm ".lsp")) nm (str nm ".lsp")),
             ret (try (with-open [in (java.io.PushbackReader. (clojure.java.io/reader nom))]
                        (binding [*read-eval* false] (try (let [res (evaluar (read in) amb-global nil)]
                                                            (cargar-arch (second res) nil in res))
                                                          (catch Exception e (imprimir nil) amb-global))))
                      (catch java.io.FileNotFoundException e (imprimir (list '*error* 'file-open-error 'file-not-found nom '1 'READ)) amb-global))]
         ret))))
  ([amb-global amb-local in res]
   (try (let [res (evaluar (read in) amb-global nil)] (cargar-arch (second res) nil in res))
        (catch Exception e (imprimir (first res)) amb-global)))
  )


(defn evaluar-quote
  "Evalua una forma 'quote' de TLC-LISP."
  [expre amb-global _]
  (if (igual? (second expre) nil)
    (list nil amb-global)
    (list (second expre) amb-global)))


(defn aplicar
  "Aplica a la lista de argumentos 'lae' la función 'fnc' en los ambientes dados."
  ([fnc lae amb-global amb-local]
   (aplicar (revisar-fnc fnc) (revisar-lae lae) fnc lae amb-global amb-local))
  ([resu1 resu2 fnc lae amb-global amb-local]
   (cond
     (error? resu1) (list resu1 amb-global)
     (error? resu2) (list resu2 amb-global)
     (not (seq? fnc)) (list (aplicar-funcion-primitiva fnc lae amb-global amb-local) amb-global)
     :else (aplicar-lambda fnc lae amb-global amb-local))))


(defn aplicar-lambda
  "Aplica la forma lambda 'fnc' a la lista de argumentos 'lae'."
  [fnc lae amb-global amb-local]
  (cond
    (< (count lae) (count (second fnc))) (list '(*error* too-few-args) amb-global)
    (> (count lae) (count (second fnc))) (list '(*error* too-many-args) amb-global)
    (nil? (next (nnext fnc))) (aplicar-lambda-simple fnc lae amb-global amb-local)
    :else (aplicar-lambda-multiple fnc lae amb-global amb-local)))


(defn aplicar-lambda-simple
  "Evalua una forma lambda 'fnc' con un cuerpo simple."
  [fnc lae amb-global amb-local]
  (let [lista-params-args (reduce concat (map list (second fnc) lae)),
        nuevo-amb-local (cond
                          (empty? amb-local) lista-params-args
                          (empty? lista-params-args) amb-local
                          :else (apply concat (apply assoc (apply assoc {} amb-local) lista-params-args)))]
    (evaluar (first (nnext fnc)) amb-global nuevo-amb-local)))


(defn aplicar-lambda-multiple
  "Evalua una forma lambda 'fnc' cuyo cuerpo contiene varias expresiones."
  [fnc lae amb-global amb-local]
  (aplicar (cons 'lambda (cons (second fnc) (next (nnext fnc))))
           lae
           (second (aplicar-lambda-simple fnc lae amb-global amb-local))  ; Nuevo ambiente global
           amb-local))


(defn aplicar-funcion-primitiva
  "Aplica una funcion primitiva a una 'lae' (lista de argumentos evaluados)."
  [fnc lae amb-global amb-local]
  (cond
    (igual? fnc 'add)     (fnc-add lae)
    (igual? fnc 'cons)    (fnc-cons lae)
    (igual? fnc 'first)   (fnc-first lae)
    (igual? fnc 'length)  (fnc-length lae)
    (igual? fnc 'list)    (fnc-list lae)
    (igual? fnc 'listp)   (fnc-listp lae)
    (igual? fnc 'not)     (fnc-not lae)
    (igual? fnc 'null)    (fnc-null lae)
    (igual? fnc 'prin3)   (fnc-prin3 lae)
    (igual? fnc 'rest)    (fnc-rest lae)
    (igual? fnc 'equal)   (fnc-equal lae)
    (igual? fnc 'read)    (fnc-read lae)
    (igual? fnc 'terpri)  (fnc-terpri lae)
    (igual? fnc 'add)    	(fnc-add lae)
    (igual? fnc 'sub)    	(fnc-sub lae)
    (igual? fnc 'lt)    	(fnc-lt lae)
    (igual? fnc 'gt)    	(fnc-gt lae)
    (igual? fnc 'ge)    	(fnc-ge lae)
    (igual? fnc 'reverse) (fnc-reverse lae)
    (igual? fnc 'env)    	(fnc-env lae amb-global amb-local)
    (igual? fnc 'append)  (fnc-append lae)

    ; Las funciones primitivas reciben argumentos y retornan un valor (son puras)

    :else (list '*error* 'non-applicable-type fnc)))


(defn fnc-cons
  "Devuelve la inserción de un elem en la cabeza de una lista."
  [lae]
  (let [ari (controlar-aridad lae 2)]
    (cond
      (seq? ari) ari
      (or (seq? (second lae)) (igual? (second lae) nil)) (cons (first lae) (second lae))
      :else (list '*error* 'not-implemented))))


(defn fnc-first
  "Devuelve el primer elemento de una lista."
  [lae]
  (let [ari (controlar-aridad lae 1)]
    (cond
      (seq? ari) ari
      (igual? (first lae) nil) nil
      (not (seq? (first lae))) (list '*error* 'list 'expected (first lae))
      :else (ffirst lae))))


(defn fnc-length
  "Devuelve la longitud de una lista."
  [lae]
  (let [ari (controlar-aridad lae 1)]
    (cond
      (seq? ari) ari
      (or (seq? (first lae)) (igual? (first lae) nil)) (count (first lae))
      :else (list '*error* 'arg-wrong-type (first lae)))))


(defn fnc-list
  "Devuelve una lista formada por los args."
  [lae]
  (if (< (count lae) 1) nil lae))


(defn fnc-listp
  "Devuelve 't' si un elemento es una lista."
  [lae]
  (let [ari (controlar-aridad lae 1)]
    (cond
      (seq? ari) ari
      (seq? (first lae)) 't
      :else nil)))


(defn fnc-not
  "Niega el argumento."
  [lae]
  (let [ari (controlar-aridad lae 1)]
    (cond
      (seq? ari) ari
      (igual? (first lae) nil) 't
      :else nil)))


(defn fnc-null
  "Devuelve 't' si un elemento es 'nil' en TLC-Lisp."
  [lae]
  (fnc-not lae))


(defn fnc-prin3
  "Imprime un elemento y lo devuelve."
  [lae]
  (cond
    (< (count lae) 1) (list '*error* 'too-few-args)
    (> (count lae) 1) (list '*error* 'not-implemented)
    (not (seq? (first lae))) (do (print (first lae)) (flush) (first lae))
    :else (do (print (map #(if (igual? % nil) nil %) (first lae))) (flush) (first lae))))


(defn fnc-rest
  "Devuelve una lista sin su 1ra. posición."
  [lae]
  (let [ari (controlar-aridad lae 1)]
    (cond
      (seq? ari) ari
      (igual? (first lae) nil) nil
      (not (seq? (first lae))) (list '*error* 'list 'expected (first lae))
      :else (nfirst lae))))


(defn imprimir
  "Imprime, con un salto de linea al final, lo recibido devolviendo
   el mismo valor. Tambien muestra los errores."
  ([elem]
   (cond
     (not (seq? elem)) (if (igual? elem \space)
                         (do (flush) elem)
                         (do (prn (if (igual? elem nil) nil elem)) (flush) elem))
     (error? elem) (imprimir elem elem)
     :else (do (prn (map #(if (igual? % nil) nil %) elem)) (flush) elem)))
  ([lis orig]
   (if (nil? lis)
     (do (prn) (flush) orig)
     (do (pr (first lis)) (print " ") (imprimir (next lis) orig)))))







; FUNCIONES QUE DEBEN SER IMPLEMENTADAS PARA COMPLETAR EL INTERPRETE DE TLC-LISP (ADEMAS DE COMPLETAR 'EVALUAR' Y 'APLICAR-FUNCION-PRIMITIVA'):

; user=> (controlar-aridad '(a b c) 3)
; 3
; user=> (controlar-aridad '(a b c) 2)
; (*error* too-many-args)
; user=> (controlar-aridad '(a b c) 4)
; (*error* too-few-args)
(defn controlar-aridad [lae long]
  "Si la longitud de una lista dada es la esperada, devuelve esa longitud.
   Si no, devuelve una lista con un mensaje de error (una lista con *error* como primer elemento)."
  (let [cant (count lae)]
    (cond
      (= cant long) long
      (> cant long) '(*error* too-many-args)
      :else '(*error* too-few-args)
      ))
  )


; user=> (igual? 1 1)
; true
; user=> (igual? 1 2)
; false
; user=> (igual? 'a 'a)
; true
; user=> (igual? 'A 'A)
; true
; user=> (igual? 'a 'A)
; true
; user=> (igual? 'A 'a)
; true
; user=> (igual? 'a 'b)
; false
; user=> (igual? '(a b c) '(A B C))
; true
; user=> (igual? '(a b c) '(A B D))
; false
; user=> (igual? nil nil)
; true
; user=> (igual? nil 'NIL)
; true
; user=> (igual? 'NIL nil)
; true
; user=> (igual? 'NIL 'NIL)
; true
; user=> (igual? nil ())
; true
; user=> (igual? 'NIL ())
; true
; user=> (igual? () ())
; true
; user=> (igual? () '(nil))
; false
; user=> (igual? "a" "a")
; true
; user=> (igual? "a" "A")
; false
; user=> (igual? 'a "a")
; false
; user=> (igual? 'a "A")
; false
(defn upper [elem]
  "Convierte un elemento (simbolo, secuencia) en mayusculas"
  (if (not (nil? elem))
    (clojure.string/upper-case elem))
  )

(defn igual? [elem1 elem2]
  "Verifica la igualdad entre dos elementos al estilo de TLC-LISP (case-insensitive)."
  (cond
    (and (symbol? elem1) (symbol? elem2)) (= (upper elem1) (upper elem2))
    (and (seq? elem1) (seq? elem2)) (= (upper elem1) (upper elem2))

    (and (nil? elem1) (= elem2 'NIL)) true
    (and (= elem1 'NIL) (nil? elem2)) true
    (and (= elem1 'NIL) (= elem2 'NIL)) true

    (and (nil? elem1) (= elem2 ())) true
    (and (= elem1 ()) (nil? elem2)) true
    (and (= elem1 'NIL) (= elem2 ())) true
    (and (= elem1 ()) (= elem2 'NIL)) true

    (and (= elem1 ()) (= elem2 ())) true

    :else (= elem1 elem2)
    )
  )


; user=> (error? '(*error* too-few-args))
; true
; user=> (error? (list '*error* 'too-few-args))
; true
; user=> (error? (list '*ERROR* 'too-few-args))
; true
; user=> (error? (list '*Error* 'too-few-args))
; true
; user=> (error? (list '*error*))
; true
; user=> (error? (list 'too-few-args))
; false
; user=> (error? '*error*)
; false
; user=> (error? ())
; false
; user=> (error? nil)
; false
(defn error? [lae]
  "Devuelve true o false, segun sea o no el arg. un mensaje de error (una lista con *error* como primer elemento)."
  (and (seq? lae) (pos? (count lae)) (igual? '*error* (first lae)))
  )


; user=> (revisar-fnc '(*error* too-few-args))
; (*error* too-few-args)
; user=> (revisar-fnc '(too-few-args))
; nil
; user=> (revisar-fnc '*error*)
; nil
; user=> (revisar-fnc nil)
; nil
; user=> (revisar-fnc ())
; nil
(defn revisar-fnc [lae]
  "Si la lista es un mensaje de error, lo devuelve; si no, devuelve nil."
  (cond
    (error? lae) lae
    :else nil
    )
  )


; user=> (revisar-lae '(1 2 3))
; nil
; user=> (revisar-lae nil)
; nil
; user=> (revisar-lae ())
; nil
; user=> (revisar-lae '(1 (*error* too-few-args) 3))
; (*error* too-few-args)
; user=> (revisar-lae '(1 (*error* too-few-args) (*error* too-many-args) 3))
; (*error* too-few-args)
(defn revisar-lae [lae]
  "Devuelve el primer elemento que es un mensaje de error. Si no hay ninguno, devuelve nil."
  (first (filter revisar-fnc lae))
  )

; user=> (actualizar-amb '(a 1 b 2 c 3) 'd 4)
; (a 1 b 2 c 3 d 4)
; user=> (actualizar-amb '(a 1 b 2 c 3) 'b 4)
; (a 1 b 4 c 3)
; user=> (actualizar-amb '(a 1 b 2 c 3) 'b (list '*error* 'mal 'hecho))
; (a 1 b 2 c 3)
; user=> (actualizar-amb () 'b 7)
; (b 7)
(defn nulo? [elem]
  (nil? elem)
  (igual? elem 'NIL)
  (igual? elem ())
  )

(defn lower-sym [elem]
  "Convierte en minusculas y en simbolos"
  (if (not (nulo? elem))
    (symbol (clojure.string/lower-case elem))
    )
  )

(defn actualizar-amb [amb clave valor]
  "Devuelve un ambiente actualizado con una clave (nombre de la variable o funcion) y su valor.
  Si el valor es un error, el ambiente no se modifica. De lo contrario, se le carga o reemplaza el valor."
  (let [claves (map first (partition 2 amb)) valores (map second (partition 2 amb))
        index (.indexOf (map lower-sym claves) (lower-sym clave))]
    (cond
      (error? valor) amb
      (>= index 0) (interleave claves (apply list (assoc (vec valores) index valor)))
      :else (concat amb (list (lower-sym clave) valor))
    ))
  )

; user=> (buscar 'c '(a 1 b 2 c 3 d 4 e 5))
; 3
; user=> (buscar 'f '(a 1 b 2 c 3 d 4 e 5))
; (*error* unbound-symbol f)
(defn buscar [clave amb]
  "Busca una clave en un ambiente (una lista con claves en las posiciones impares [1, 3, 5...] y valores en las pares [2, 4, 6...]
   y devuelve el valor asociado. Devuelve un mensaje de error si no la encuentra."
  (let [claves (map first (partition 2 amb)) valores (map second (partition 2 amb))
        index (.indexOf (map lower-sym claves) (lower-sym clave))]
    (cond
      (>= index 0) (nth valores index)
      :else (list '*error* 'unbound-symbol clave)
      ))
  )

; user=> (fnc-append '( (1 2) ))
; (*error* too-few-args)
; user=> (fnc-append '( (1 2) (3) (4 5) (6 7) ))
; (*error* too-many-args)
; user=> (fnc-append '( (1 2) 3 ))
; (*error* list expected 3)
; user=> (fnc-append '( (1 2) A ))
; (*error* list expected A)
; user=> (fnc-append '( (1 2) (3)))
; (1 2 3)
; user=> (fnc-append '( (1 2) nil ))
; (1 2)
; user=> (fnc-append '( () (1 2) ))
; (1 2)
; user=> (fnc-append '(nil nil))
; nil
; user=> (fnc-append '(() ()))
; nil
(defn fnc-append [lae]
  "Devuelve el resultado de fusionar 2 sublistas."
  (let [ari (controlar-aridad lae 2) prim (first lae) seg (second lae)]
    (cond
      (error? ari) ari
      (and (empty? prim) (empty? seg)) nil
      (and (seq? prim) (seq? seg)) (concat prim seg)
      (and (nulo? prim) (not (nulo? seg))) seg
      (and (not (nulo? prim)) (nulo? seg)) prim

      (and (seq? prim) (not (seq? seg))) (list '*error* 'list 'expected seg)
      (and (not (seq? prim)) (seq? seg)) (list '*error* 'list 'expected prim)
      :else nil
      ))
  )

; user=> (fnc-env () '(a 1 b 2) '(c 3 d 4))
; (a 1 b 2 c 3 d 4)
; user=> (fnc-env '(5) '(a 1 b 2) '(c 3 d 4))
; (*error* too-many-args)
(defn fnc-env [lae amb-global amb-local]
  "Devuelve la fusion de los ambientes global y local."
  (cond
    (not (empty? lae)) '(*error* too-many-args)
    :else (concat amb-global amb-local)
    )
  )

; user=> (fnc-equal '(1 1))
; t
; user=> (fnc-equal '(A a))
; t
; user=> (fnc-equal '("1" "1"))
; t
; user=> (fnc-equal '(nil NIL))
; t
; user=> (fnc-equal '(1 2))
; nil
; user=> (fnc-equal '(A B))
; nil
; user=> (fnc-equal '("1" 1))
; nil
; user=> (fnc-equal ())
; (*error* too-few-args)
; user=> (fnc-equal '(A))
; (*error* too-few-args)
; user=> (fnc-equal '(A a A))
; (*error* too-many-args)
(defn fnc-equal [lae]
  "Compara 2 elementos. Si son iguales, devuelve t. Si no, nil."
  (let [ari (controlar-aridad lae 2)]
    (cond
      (error? ari) ari
      (igual? (first lae) (second lae)) 't
      :else nil
      ))
  )

; user=> (fnc-read ())
; 1
; 1
; user=> (fnc-read ())
; a
; a
; user=> (fnc-read ())
; "hola"
; "hola"
; user=> (fnc-read ())
; (hola mundo)
; (hola mundo)
; user=> (fnc-read ())
; (hola
; mundo)
; (hola mundo)
; user=> (fnc-read ())
; ()
; nil
; user=> (fnc-read ())
; nil
; nil
; user=> (fnc-read '(1))
; (*error* not-implemented)
; user=> (fnc-read '(1 2))
; (*error* not-implemented)
(defn fnc-read [lae]
  "Devuelve la lectura de un elemento de TLC-LISP desde la terminal/consola."
  (cond
    (empty? lae) (read)
    :else '(*error* not-implemented)
    )
  )

; user=> (fnc-terpri ())
;
; nil
; user=> (fnc-terpri '(1))
; (*error* not-implemented)
; user=> (fnc-terpri '(1 2))
; (*error* not-implemented)
(defn fnc-terpri [lae]
  "Imprime un salto de línea y devuelve nil."
  (cond
    (empty? lae) (do (println) nil)
    :else '(*error* not-implemented)
    )
  )

; user=> (fnc-add ())
; (*error* too-few-args)
; user=> (fnc-add '(3))
; (*error* too-few-args)
; user=> (fnc-add '(3 4))
; 7
; user=> (fnc-add '(3 4 5))
; 12
; user=> (fnc-add '(3 4 5 6))
; 18
; user=> (fnc-add '(A 4 5 6))
; (*error* number-expected A)
; user=> (fnc-add '(3 A 5 6))
; (*error* number-expected A)
; user=> (fnc-add '(3 4 A 6))
; (*error* number-expected A)
(defn fnc-add [lae]
  "Suma los elementos de una lista. Minimo 2 elementos."
  (let [simbolos (filter symbol? lae)]
    (cond
      (< (count lae) 2) '(*error* too-few-args)
      (> (count simbolos) 0) (list '*error* 'number-expected (first simbolos))
      :else (apply + lae)
      ))
  )

; user=> (fnc-sub ())
; (*error* too-few-args)
; user=> (fnc-sub '(3))
; -3
; user=> (fnc-sub '(3 4))
; -1
; user=> (fnc-sub '(3 4 5))
; -6
; user=> (fnc-sub '(3 4 5 6))
; -12
; user=> (fnc-sub '(A 4 5 6))
; (*error* number-expected A)
; user=> (fnc-sub '(3 A 5 6))
; (*error* number-expected A)
; user=> (fnc-sub '(3 4 A 6))
; (*error* number-expected A)
(defn fnc-sub [lae]
  "Resta los elementos de un lista. Minimo 1 elemento."
  (let [simbolos (filter symbol? lae)]
    (cond
      (empty? lae) '(*error* too-few-args)
      (> (count simbolos) 0) (list '*error* 'number-expected (first simbolos))
      :else (apply - lae)
      ))
  )

; user=> (fnc-lt ())
; (*error* too-few-args)
; user=> (fnc-lt '(1))
; (*error* too-few-args)
; user=> (fnc-lt '(1 2))
; t
; user=> (fnc-lt '(1 1))
; nil
; user=> (fnc-lt '(2 1))
; nil
; user=> (fnc-lt '(A 1))
; (*error* number-expected A)
; user=> (fnc-lt '(1 A))
; (*error* number-expected A)
; user=> (fnc-lt '(1 2 3))
; (*error* too-many-args)
(defn fnc-lt [lae]
  "Devuelve t si el primer numero es menor que el segundo; si no, nil."
  (let [cant (count lae) prim (first lae) seg (second lae)]
    (cond
      (< cant 2) '(*error* too-few-args)
      (> cant 2) '(*error* too-many-args)
      (symbol? prim) (list '*error* 'number-expected prim)
      (symbol? seg) (list '*error* 'number-expected seg)
      (< prim seg) 't
      :else nil
      ))
  )

; user=> (fnc-gt ())
; (*error* too-few-args)
; user=> (fnc-gt '(1))
; (*error* too-few-args)
; user=> (fnc-gt '(2 1))
; t
; user=> (fnc-gt '(1 1))
; nil
; user=> (fnc-gt '(1 2))
; nil
; user=> (fnc-gt '(A 1))
; (*error* number-expected A)
; user=> (fnc-gt '(1 A))
; (*error* number-expected A)
; user=> (fnc-gt '(1 2 3))
; (*error* too-many-args)
(defn fnc-gt [lae]
  "Devuelve t si el primer numero es mayor que el segundo; si no, nil."
  (let [cant (count lae) prim (first lae) seg (second lae)]
    (cond
      (< cant 2) '(*error* too-few-args)
      (> cant 2) '(*error* too-many-args)
      (symbol? prim) (list '*error* 'number-expected prim)
      (symbol? seg) (list '*error* 'number-expected seg)
      (> prim seg) 't
      :else nil
      ))
  )

; user=> (fnc-ge ())
; (*error* too-few-args)
; user=> (fnc-ge '(1))
; (*error* too-few-args)
; user=> (fnc-ge '(2 1))
; t
; user=> (fnc-ge '(1 1))
; t
; user=> (fnc-ge '(1 2))
; nil
; user=> (fnc-ge '(A 1))
; (*error* number-expected A)
; user=> (fnc-ge '(1 A))
; (*error* number-expected A)
; user=> (fnc-ge '(1 2 3))
; (*error* too-many-args)
(defn fnc-ge [lae]
  "Devuelve t si el primer numero es mayor o igual que el segundo; si no, nil."
  (let [cant (count lae) prim (first lae) seg (second lae)]
    (cond
      (< cant 2) '(*error* too-few-args)
      (> cant 2) '(*error* too-many-args)
      (symbol? prim) (list '*error* 'number-expected prim)
      (symbol? seg) (list '*error* 'number-expected seg)
      (>= prim seg) 't
      :else nil
      ))
  )

; user=> (fnc-reverse ())
; (*error* too-few-args)
; user=> (fnc-reverse '(1))
; (*error* list expected 1)
; user=> (fnc-reverse '(A))
; (*error* list expected A)
; user=> (fnc-reverse '((1)) )
; (1)
; user=> (fnc-reverse '((1 2 3)) )
; (3 2 1)
; user=> (fnc-reverse '((1 2 3)(4)) )
; (*error* too-many-args)
(defn fnc-reverse [lae]
  "Devuelve una lista con sus elementos en orden inverso."
  (let [prim (first lae) cant (count lae)]
    (cond
      (empty? lae) '(*error* too-few-args)
      (> cant 1) '(*error* too-many-args)
      (seq? prim) (reverse prim)
      :else (list '*error* 'list 'expected prim)
      ))
  )

; user=> (evaluar-escalar 32 '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))
; (32 (v 1 w 3 x 6))
; user=> (evaluar-escalar "chau" '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))
; ("chau" (v 1 w 3 x 6))
; user=> (evaluar-escalar 'z '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))
; ("hola" (v 1 w 3 x 6))
; user=> (evaluar-escalar 'Z '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))
; ("hola" (v 1 w 3 x 6))
; user=> (evaluar-escalar 'w '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))
; (3 (v 1 w 3 x 6))
; user=> (evaluar-escalar 'x '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))
; (5 (v 1 w 3 x 6))
; user=> (evaluar-escalar 'n '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))
; ((*error* unbound-symbol n) (v 1 w 3 x 6))
(defn evaluar-escalar [expre amb-global amb-local]
  "Evalua una expresion escalar consultando, si corresponde, los ambientes local y global. Devuelve una lista con el resultado y un ambiente."
  (let [busq (buscar (lower-sym expre) amb-local)]
    (cond
      (not (symbol? expre)) (list expre amb-global)

      (and (error? busq) (error? (buscar (lower-sym expre) amb-global))) (list (list '*error* 'unbound-symbol expre) amb-global)
      (not (error? busq)) (list busq amb-global)

      :else (list (buscar (lower-sym expre) amb-global) amb-global)
      ))
  )

; user=> (evaluar-de '(de f (x)) '(x 1))
; (f (x 1 f (lambda (x))))
; user=> (evaluar-de '(de f (x) 2) '(x 1))
; (f (x 1 f (lambda (x) 2)))
; user=> (evaluar-de '(de f (x) (+ x 1)) '(x 1))
; (f (x 1 f (lambda (x) (+ x 1))))
; user=> (evaluar-de '(de f (x y) (+ x y)) '(x 1))
; (f (x 1 f (lambda (x y) (+ x y))))
; user=> (evaluar-de '(de f (x y) (prin3 x) (terpri) y) '(x 1))
; (f (x 1 f (lambda (x y) (prin3 x) (terpri) y)))
; user=> (evaluar-de '(de) '(x 1))
; ((*error* list expected nil) (x 1))
; user=> (evaluar-de '(de f) '(x 1))
; ((*error* list expected nil) (x 1))
; user=> (evaluar-de '(de f 2) '(x 1))
; ((*error* list expected 2) (x 1))
; user=> (evaluar-de '(de f 2 3) '(x 1))
; ((*error* list expected 2) (x 1))
; user=> (evaluar-de '(de (f)) '(x 1))
; ((*error* list expected nil) (x 1))
; user=> (evaluar-de '(de 2 x) '(x 1))
; ((*error* list expected x) (x 1))
; user=> (evaluar-de '(de 2 (x)) '(x 1))
; ((*error* symbol expected 2) (x 1))
; user=> (evaluar-de '(de nil (x) 2) '(x 1))
; ((*error* cannot-set nil) (x 1))
(defn evaluar-de [expre amb-global]
  "Evalua una forma 'de'. Devuelve una lista con el resultado y un ambiente actualizado con la definicion."
  (let [cant (count expre) f (second expre)]
    (cond
      (< cant 3) (list '(*error* list expected nil) amb-global)
      (nulo? f) (list '(*error* cannot-set nil) amb-global)
      (and (> cant 2) (not (seq? (nth expre 2)))) (list (list '*error* 'list 'expected (nth expre 2)) amb-global)
      (and (< cant 4) (not (symbol? f))) (list (list '*error* 'symbol 'expected f) amb-global)

      :else (list f (actualizar-amb amb-global f (concat '(lambda) (drop 2 expre))))
      ))
  )


; user=> (evaluar-if '(if t) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))
; (nil (nil nil t t v 1 w 3 x 6))
; user=> (evaluar-if '(if 7) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))
; (nil (nil nil t t v 1 w 3 x 6))
; user=> (evaluar-if '(if nil) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))
; (nil (nil nil t t v 1 w 3 x 6))
; user=> (evaluar-if '(if x) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))
; (nil (nil nil t t v 1 w 3 x 6))
; user=> (evaluar-if '(if t 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))
; (9 (nil nil t t v 1 w 3 x 6))
; user=> (evaluar-if '(if z 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))
; (9 (nil nil t t v 1 w 3 x 6))
; user=> (evaluar-if '(if w 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))
; (9 (nil nil t t v 1 w 3 x 6))
; user=> (evaluar-if '(if r 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))
; ((*error* unbound-symbol r) (nil nil t t v 1 w 3 x 6))
; user=> (evaluar-if '(if nil 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))
; (nil (nil nil t t v 1 w 3 x 6))
; user=> (evaluar-if '(if nil 9 z) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))
; ("hola" (nil nil t t v 1 w 3 x 6))
; user=> (evaluar-if '(if nil 9 1 2 3 z) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))
; ("hola" (nil nil t t v 1 w 3 x 6))
; user=> (evaluar-if '(if nil 9 w) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))
; (3 (nil nil t t v 1 w 3 x 6))
; user=> (evaluar-if '(if nil 9 8) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))
; (8 (nil nil t t v 1 w 3 x 6))
; user=> (evaluar-if '(if nil a 8) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))
; (8 (nil nil t t v 1 w 3 x 6))
; user=> (evaluar-if '(if (gt 2 0) a 8) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))
; ((*error* unbound-symbol a) (gt gt nil nil t t v 1 w 3 x 6))
; user=> (evaluar-if '(if (gt 0 2) a 8) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))
; (8 (gt gt nil nil t t v 1 w 3 x 6))
; user=> (evaluar-if '(if (gt 0 2) a (setq m 8)) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))
; (8 (gt gt nil nil t t v 1 w 3 x 6 m 8))
(defn evaluar-if [expre amb-global amb-local]
  "Evalua una forma 'if'. Devuelve una lista con el resultado y un ambiente eventualmente modificado."
  (let [cant (count expre)]
    (cond
      (< cant 3) (list nil amb-global)
      (and (= cant 3) (error? (first (evaluar (second expre) amb-global amb-local)))) (evaluar (second expre) amb-global amb-local)
      (and (= cant 3) (not (nulo? (second expre)))(not (error? (first (evaluar (second expre) amb-global amb-local))))) (evaluar (last expre) amb-global amb-local)
      (and (= cant 3) (nulo? (second expre)) (not (symbol? (last expre)))) (list nil amb-global)

      (igual? 't (first (evaluar (second expre) amb-global amb-local))) (evaluar (nth expre 2) amb-global amb-local)
      (>= cant 3) (evaluar (last expre) amb-global amb-local)
      (nulo? (first (evaluar (second expre) amb-global amb-local))) (evaluar (nth expre 3) amb-global amb-local)
      )
    )
  )


; user=> (evaluar-or '(or) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))
; (nil (nil nil t t w 5 x 4))
; user=> (evaluar-or '(or nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))
; (nil (nil nil t t w 5 x 4))
; user=> (evaluar-or '(or t) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))
; (t (nil nil t t w 5 x 4))
; user=> (evaluar-or '(or w) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))
; (5 (nil nil t t w 5 x 4))
; user=> (evaluar-or '(or r) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))
; ((*error* unbound-symbol r) (nil nil t t w 5 x 4))
; user=> (evaluar-or '(or y) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))
; (nil (nil nil t t w 5 x 4))
; user=> (evaluar-or '(or 6) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))
; (6 (nil nil t t w 5 x 4))
; user=> (evaluar-or '(or nil 6) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))
; (6 (nil nil t t w 5 x 4))
; user=> (evaluar-or '(or (setq b 8) nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))
; (8 (nil nil t t w 5 x 4 b 8))
; user=> (evaluar-or '(or nil 6 nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))
; (6 (nil nil t t w 5 x 4))
; user=> (evaluar-or '(or nil 6 r nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))
; (6 (nil nil t t w 5 x 4))
; user=> (evaluar-or '(or nil t r nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))
; (t (nil nil t t w 5 x 4))
; user=> (evaluar-or '(or nil nil nil nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))
; (nil (nil nil t t w 5 x 4))
(defn devolver-elem [elem]
  (if (not (nulo? elem)) elem nil
    )
  )


(defn mi-or [elem1 elem2 amb-global amb-local]
  (or (devolver-elem (first (evaluar elem1 amb-global amb-local))) (devolver-elem (first (evaluar elem2 amb-global amb-local))))
  )

(defn evaluar-or [expre amb-global amb-local]
  "Evalua una forma 'or'. Devuelve una lista con el resultado y un ambiente."
  (let [cant (count expre)]
    (cond
      (= 1 cant) (list nil amb-global)
      (= 2 cant) (evaluar (second expre) amb-global amb-local)
      :else (list (reduce (fn [elem1 elem2] (mi-or elem1 elem2 amb-global amb-local)) (pop expre)) amb-global)
      )
    )
  )


; user=> (evaluar-setq '(setq) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))
; ((*error* list expected nil) (nil nil t t + add w 5 x 4))
; user=> (evaluar-setq '(setq m) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))
; ((*error* list expected nil) (nil nil t t + add w 5 x 4))
; user=> (evaluar-setq '(setq m 7) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))
; (7 (nil nil t t + add w 5 x 4 m 7))
; user=> (evaluar-setq '(setq x 7) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))
; (7 (nil nil t t + add w 5 x 7))
; user=> (evaluar-setq '(setq x (+ x 1)) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))
; (2 (nil nil t t + add w 5 x 2))
; user=> (evaluar-setq '(setq x (+ x 1)) '(nil nil t t + add w 5 x 4) '(y nil z 3))
; (5 (nil nil t t + add w 5 x 5))
; user=> (evaluar-setq '(setq nil) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))
; ((*error* list expected nil) (nil nil t t + add w 5 x 4))
; user=> (evaluar-setq '(setq nil 7) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))
; ((*error* cannot-set nil) (nil nil t t + add w 5 x 4))
; user=> (evaluar-setq '(setq 7 8) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))
; ((*error* symbol expected 7) (nil nil t t + add w 5 x 4))
; user=> (evaluar-setq '(setq x 7 m (+ x 7)) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))
; (8 (nil nil t t + add w 5 x 7 m 8))
; user=> (evaluar-setq '(setq x 7 m (+ x 7)) '(nil nil t t + add w 5 x 4) '(y nil z 3))
; (14 (nil nil t t + add w 5 x 7 m 14))
; user=> (evaluar-setq '(setq x 7 y) '(nil nil t t + add w 5 x 4) '(y nil z 3))
; ((*error* list expected nil) (nil nil t t + add w 5 x 7))
; user=> (evaluar-setq '(setq x 7 y 8 z 9) '(nil nil t t + add w 5 x 4) '(y nil z 3))
; (9 (nil nil t t + add w 5 x 7 y 8 z 9))
(defn evaluar-setq [expre amb-global amb-local]
  "Evalua una forma 'setq'. Devuelve una lista con el resultado y un ambiente actualizado."

  ;(if (= (count expre) 1) (list (list '*error* 'list 'expected nil) amb-global))

  (let [cant (count expre) res (evaluar (nth expre 2) amb-global amb-local) seg (second expre)]
    (cond
      (and (not (symbol? seg)) (not (nulo? seg))) (list (list '*error* 'symbol 'expected seg) amb-global)
      (and (> cant 2) (nulo? seg)) (list (list '*error* 'cannot-set nil) amb-global)
      (error? (first res)) res
      :else (if (not (seq? (nnext (next expre))))
              (list (first res) (actualizar-amb (second res) seg (first res)))
              (recur (nnext expre) (actualizar-amb (second res) seg (first res)) amb-local))

      )))


; Al terminar de cargar el archivo en el REPL de Clojure (con load-file), se debe devolver true.