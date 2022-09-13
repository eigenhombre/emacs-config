;; Gaming stuff: NOT currently wired into init.el!

(defun d6 () (+ 1 (random 6)))
(defun d2.6 () (+ (d6) (d6)))
(defun as-h (n) (format "%X" n))

(defun uxp (n)
  (apply 'concat (cl-loop repeat n
                          collect (as-h (+ (random 6)
                                           (random 6)
                                           2)))))
(defun uwp () (uxp 7))
(defun upp () (uxp 6))

;; Name generation stuff: move elsewhere!
(require 'dash)
(require 'a)       ;; Clojure-like alist semantics
(require 'cl-lib)

(defun take (n coll)
  (cl-loop for el in coll
           repeat n
           collect el))

(defun clj-partition (n step coll)
  (when-let ((s coll))
    (let ((p (take n s)))
      (when (= n (length p))
        (cons p (clj-partition n step (nthcdr step s)))))))

(clj-partition 3 1 '(1 2 3 4))
;;=>
'((1 2 3) (2 3 4))

(setq source-names '(aimee aim air aid))

(seq-random-elt source-names)
(defun make-raw-chain (coll)
  (cl-loop for n in coll
           append
           (cl-loop for x in
                    (clj-partition 3 1 (let ((ns (symbol-name n)))
                                         (append (cl-loop for elt across ns
                                                          collect (char-to-string elt))
                                                 '(stop))))
                    collect (list (take 2 x) (caddr x)))))

(make-raw-chain source-names)
;;=>
'((("a" "i") "m")
  (("i" "m") "e")
  (("m" "e") "e")
  (("e" "e") stop)
  (("a" "i") "m")
  (("i" "m") stop)
  (("a" "i") "r")
  (("i" "r") stop)
  (("a" "i") "d")
  (("i" "d") stop))

(defun repeat (n x)
  (cl-loop repeat n collect x))

(defun frequencies (coll)
  "Like Clojure `frequencies`: https://emacs.stackexchange.com
   /questions/13514/
   how-to-obtain-the-statistic-of-the-the-frequency-of-words-in-a-buffer"
  (cl-loop with result = nil
           for elt in coll
           do (cl-incf (cdr (or (assoc elt result)
                                (car (push (cons elt 0) result)))))
           finally return result))

(frequencies '("a" "bb" "BB" "bb" "aa" "a"))

(frequencies (make-raw-chain source-names))
;;=>
'(((("i" "d") stop) . 1)
  ((("a" "i") "d") . 1)
  ((("i" "r") stop) . 1)
  ((("a" "i") "r") . 1)
  ((("i" "m") stop) . 1)
  ((("e" "e") stop) . 1)
  ((("m" "e") "e") . 1)
  ((("i" "m") "e") . 1)
  ((("a" "i") "m") . 2))

;; (let ((ret nil))
;;   (cl-loop for ((ngram nxt) . n) in (frequencies (make-raw-chain source-names))
;;            do (let (cur ()))))

(cl-loop
 for ((ngram nxt) . n) in (frequencies (make-raw-chain source-names))
 collect (list ngram nxt n))
;;=>
'((("i" "d") stop 1)
  (("a" "i") "d" 1)
  (("i" "r") stop 1)
  (("a" "i") "r" 1)
  (("i" "m") stop 1)
  (("e" "e") stop 1)
  (("m" "e") "e" 1)
  (("i" "m") "e" 1)
  (("a" "i") "m" 2))

(defun group-ngram-transitions (transitions)
  (let ((grouped-transitions
         (-group-by #'car (cl-loop
                           for ((ngram nxt) . n) in (frequencies transitions)
                           collect (list ngram nxt n)))))
    (cl-loop for (ngram . coll) in grouped-transitions
             collect (cons ngram (cl-loop for (_ b c) in coll
                                          collect (cons b c))))))

(group-ngram-transitions (make-raw-chain source-names))
;;=>
'((("i" "d")
   (stop . 1))
  (("a" "i")
   ("d" . 1)
   ("r" . 1)
   ("m" . 2))
  (("i" "r")
   (stop . 1))
  (("i" "m")
   (stop . 1)
   ("e" . 1))
  (("e" "e")
   (stop . 1))
  (("m" "e")
   ("e" . 1)))

(defun pick-entry (entries)
  (let* ((n (cl-loop for (k . v) in entries sum v))
         (r (random n))
         (pos 0))
    (cl-loop named foo
             for (k . v) in entries
             do (setq pos (+ v pos))
             when (> pos r)
             return k)))

(defun choose-for-ngram (entries ngram)
  (pick-entry (a-get entries ngram)))

(defun sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
    See `sort-words'.  See https://www.emacswiki.org/emacs/SortWords"
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))


(setq more-names '(
                   aaliyah aarya abby abigail ada adalee adaline adalyn adalynn addilyn
                   addilynn addison addisyn addyson adelaide adele adelina adeline adelyn
                   adelynn adley adriana adrianna aila ailani aileen ainhoa ainsley aisha
                   aislinn aitana alaia alaina alaiya alana alani alanna alaya alayah
                   alayna aleah aleena alejandra alena alessandra alessia alexa alexandra
                   alexandria alexia alexis alia aliana alianna alice alicia alina alison
                   alisson alivia aliya aliyah aliza allie allison allyson alma alondra
                   alora alyssa amaia amalia amanda amani amara amari amaris amaya amayah
                   amber amelia amelie amina amira amirah amiyah amora amoura amy ana
                   anahi anais analia anastasia anaya andi andrea angel angela angelica
                   angelina angie anika aniya aniyah anna annabella annabelle annalise
                   anne annie annika ansley antonella anya april arabella araceli ari
                   aria ariah ariana arianna ariel ariella arielle ariya ariyah armani
                   artemis arya ashley ashlyn ashlynn aspen aspyn astrid athena aubree
                   aubrey aubrie aubriella aubrielle audrey august aurelia aurora autumn
                   ava avah avalynn avayah averi averie avery aviana avianna aya ayla
                   ayleen aylin azalea azariah bailee bailey barbara baylee beatrice
                   belen bella bellamy belle berkley bethany bexley bianca blair blaire
                   blake blakely bonnie braelyn braelynn braylee bria briana brianna
                   briar bridget briella brielle brinley bristol brittany brooke brooklyn
                   brooklynn brylee brynlee brynleigh brynn cadence cali callie calliope
                   cameron camila camilla camille camryn capri cara carly carmen carolina
                   caroline carolyn carter cassandra cassidy cataleya catalina catherine
                   cecelia cecilia celeste celia celine chana chandler chanel charlee
                   charleigh charley charli charlie charlotte chaya chelsea cheyenne
                   chloe christina claire clara clare clarissa clementine cleo colette
                   collins cora coraline corinne crystal cynthia dahlia daisy dakota
                   dalary daleyza dallas dani daniela daniella danielle danna daphne
                   davina dayana deborah delaney delilah della demi denise denisse denver
                   destiny diana dior dorothy dream drew dulce dylan eden edith egypt
                   eileen elaina elaine eleanor elena eliana elianna elina elisa
                   elisabeth elise eliza elizabeth ella elle ellen elliana ellianna ellie
                   elliot elliott ellis ellison elodie eloise elora elsa elsie elyse
                   emani ember emberly emelia emely emerald emerie emerson emersyn emery
                   emilia emily emma emmaline emmalyn emmalynn emmeline emmie emmy emory
                   ensley erin esme esmeralda esperanza estella estelle esther estrella
                   etta eva evangeline eve evelyn evelynn everlee everleigh everly evie
                   ezra faith fallon fatima faye felicity fernanda finley fiona flora
                   florence frances francesca frankie freya freyja frida gabriela
                   gabriella gabrielle galilea gemma genesis genevieve georgia gia giana
                   gianna giavanna giovanna giselle giuliana gloria grace gracelyn
                   gracelynn gracie greta guadalupe gwen gwendolyn hadassah hadlee
                   hadleigh hadley hailey haisley haley halle hallie halo hana hanna
                   hannah harlee harleigh harley harlow harmoni harmony harper hattie
                   haven hayden haylee hayley hazel heaven heidi helen helena henley
                   holland holly hope hunter ila iliana imani indie irene iris isabel
                   isabela isabella isabelle isla itzayana itzel ivanna ivory ivy iyla
                   izabella jacqueline jada jade jaelynn jaliyah jamie jane janelle
                   janiyah jasmine jaycee jayda jayla jaylah jaylee jayleen jaylin jazlyn
                   jazmin jazmine jemma jenesis jenna jennifer jessica jessie jianna
                   jillian jimena joanna jocelyn joelle johanna jolene jolie jordan
                   jordyn josephine josie journee journey journi jovie joy joyce judith
                   julia juliana julianna julie juliet julieta juliette julissa june
                   juniper justice kadence kai kaia kailani kailey kairi kaisley kaitlyn
                   kaiya kalani kali kaliyah kallie kamari kamila kamilah kamiyah kamryn
                   kara karen karina karla karsyn karter kassidy kataleya katalina kate
                   katelyn katherine kathryn katie kaydence kayla kaylani kaylee kayleigh
                   kaylie kehlani keilani keily keira kelly kelsey kendall kendra kenia
                   kenna kennedi kennedy kensley kenzie keyla khalani khaleesi khloe
                   kiana kiara kimber kimberly kimora kinley kinslee kinsley kira kora
                   kori kyla kylee kyleigh kylie kynlee kyra lacey laila lainey lana
                   landry laney lara laura laurel lauren lauryn layla laylah laylani
                   layne lea leah leanna legacy leia leighton leila leilani leilany lena
                   lennon lennox leona leslie lexi lexie leyla lia liana liberty lila
                   lilah lilian liliana lilianna lilith lillian lilliana lillie lilly
                   lily lilyana lina linda liv livia logan lola london londyn lorelai
                   lorelei loretta louisa louise lucia luciana lucille lucy luella luisa
                   luna lyanna lydia lyla lylah lyra lyric mabel maci macie mackenzie
                   macy madalyn madalynn maddison madeleine madeline madelyn madelynn
                   madilyn madilynn madison madisyn mae maeve magdalena maggie magnolia
                   maia maisie makayla makenna makenzie malani malaya malayah malaysia
                   maleah malia maliyah mallory mara marceline maren margaret margo
                   margot maria mariah mariam mariana marianna marie marilyn marina
                   marisol marlee marleigh marley marlowe martha mary maryam matilda
                   mavis maxine maya mazikeen mckenna mckenzie mckinley meadow megan
                   meghan meilani melani melanie melany melina melissa melody mercy
                   meredith mia michaela michelle mikaela mikayla mila milan milana
                   milani milena miley millie mina mira miracle miranda miriam molly
                   monica monroe morgan murphy mya myla mylah myra nadia nala nalani
                   nancy naomi natalia natalie nataly natasha nathalia nathalie navy naya
                   nayeli nellie nevaeh nia nicole nina noa noah noelle noemi nola noor
                   nora norah nova novah novalee nyla nylah nyomi oaklee oakleigh oakley
                   oaklyn oaklynn octavia olive olivia opal ophelia paige paislee
                   paisleigh paisley paityn palmer paloma paola paris parker paula
                   paulina payton pearl penelope penny persephone peyton phoebe phoenix
                   piper poppy presley princess priscilla promise queen quinn rachel
                   raegan raelyn raelynn raina ramona raquel raven raya rayna rayne
                   reagan rebecca rebekah reese regina reign reina remi remington remy
                   renata reyna rhea riley river rivka robin romina rory rosa rosalee
                   rosalia rosalie rosalyn rose roselyn rosemary rosie rowan royal
                   royalty ruby ruth ryan ryann rylan rylee ryleigh rylie sabrina sadie
                   sage saige salem samantha samara samira saoirse sara sarah sarai
                   sariah sariyah sasha savanna savannah sawyer saylor scarlet scarlett
                   scarlette scout selah selena selene serena serenity sevyn shay shelby
                   shiloh siena sienna sierra simone sky skye skyla skylar skyler sloan
                   sloane sofia sophia sophie stella stephanie stevie stormi summer sunny
                   sutton sydney sylvia sylvie talia tatum taylor teagan teresa tessa
                   thalia thea theodora tiana tiffany tinsley tori treasure trinity vada
                   valentina valeria valerie valery vanessa veda vera veronica victoria
                   vienna violet violeta violette virginia vivian viviana vivienne
                   waverly whitley whitney willa willow winnie winter wren wrenley wynter
                   ximena xiomara yamileth yara yareli yaretzi yasmin zahra zainab
                   zaniyah zara zaria zariah zariyah zaylee zelda zendaya zhuri zoe zoey
                   zoie zola zora zoya zuri))

;; (setq eval-expression-print-length 100000)

(defvar grouped-transitions (group-ngram-transitions (make-raw-chain more-names)))

(defun generate-name (arg)
  (cl-loop with cur = (reverse arg)
           with ret = cur
           with stop
           with next
           do (progn
                (setq next (choose-for-ngram grouped-transitions
                                             (reverse cur)))
                (if
                    (or (equal next 'stop)
                        (null next))
                    (setq stop t)
                  (progn
                    (setq ret (cons next ret))
                    (setq cur (cons next (take 1 cur))))))
           until stop
           finally (return (apply #'concat (reverse ret)))))

(comment
 (cl-loop repeat 10 collect (generate-name '("a" "i"))))

(defun namegen-char-behind-point-as-lc-string (n-chars-back)
  (char-to-string (downcase (char-after (- (point) n-chars-back)))))

(defun namegen-seed-from-point ()
  (list (namegen-char-behind-point-as-lc-string 2)
        (namegen-char-behind-point-as-lc-string 1)))

(defvar namegen-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Turn the mode off:
    (define-key map (kbd "M-p")
      (lambda ()
        (interactive)
        (namegen-mode 'toggle)))
    ;; "grow" name starting at point based on previous two characters:
    (define-key map (kbd "<tab>")
      (lambda ()
        (interactive)
        (insert (substring (generate-name (namegen-seed-from-point)) 2))))
    map))

(define-minor-mode namegen-mode
  "Programmatic name generation"
  :init-value nil
  :lighter " Namegen"
  :keymap namegen-mode-map
  :group 'namegen)

(global-set-key (kbd "C-o M-p") (lambda ()
                                  (interactive)
                                  (namegen-mode t)))

(comment
 (namegen-mode 'toggle))
