set_option autoImplicit false

/------------------------------------------------------------------------------
 ## Naravna števila

 Definirajte funkcijo, ki _rekurzivno_ (torej naivno in ne direktno s formulo,
 ki jo boste morali dokazati) sešteje prvih `n` naravnih števil, ter
 dokažite, da zanjo velja znana enakost (najprej v obliki, ki ne zahteva
 deljenja, nato pa še v običajni obliki).
------------------------------------------------------------------------------/
def vsota_prvih : Nat → Nat :=
fun n =>match n with
  |Nat.zero=> 0
  |Nat.succ k=> (vsota_prvih k) + Nat.succ k

theorem gauss : (n : Nat) → 2 * vsota_prvih n = n * (n + 1) :=
  by
    intro n
    induction n with
    |zero => simp [vsota_prvih]
    |succ h ih =>
    simp [vsota_prvih]
    rw [Nat.mul_add, Nat.add_mul, Nat.one_mul, ih]
    simp [Nat.mul_add, Nat.add_assoc]
    rw [Nat.add_comm, Nat.mul_comm, Nat.add_assoc]
    simp [Nat.add_comm]




theorem cisto_pravi_gauss : (n : Nat) → vsota_prvih n = (n * (n + 1)) / 2 := by
  intro n
  calc vsota_prvih n = 2 *(vsota_prvih n) / 2 := by simp
       _ = n * (n + 1) / 2  := by rw [gauss]

/------------------------------------------------------------------------------
 ## Vektorji

 Definirajmo vektorje podobno kot na predavanjih, le da namesto svojih naravnih
 števil uporabimo vgrajena. Da se tipi ujamejo, funkcijo stikanja napišemo s
 pomočjo taktik.

 Napišite funkcijo `obrni`, ki vrne na glavo obrnjen vektor, ter funkciji
 `glava` in `rep`, ki varno vrneta glavo in rep _nepraznega_ seznama.
------------------------------------------------------------------------------/

inductive Vektor : Type → Nat → Type where
  | prazen : {A : Type} → Vektor A 0
  | sestavljen : {A : Type} → {n : Nat} → A → Vektor A n → Vektor A (n + 1)
deriving Repr

def stakni : {A : Type} → {m n : Nat} → Vektor A m → Vektor A n → Vektor A (m + n) :=
  fun xs ys => match xs with
  | .prazen => by rw [Nat.add_comm]; exact ys
  | .sestavljen x xs' => by rw [Nat.add_right_comm]; exact Vektor.sestavljen x (stakni xs' ys)

def obrni : {A : Type} → {n : Nat} → Vektor A n → Vektor A n :=
  fun vec => match vec with
  | Vektor.prazen => Vektor.prazen
  | Vektor.sestavljen x xs => stakni (obrni xs) (Vektor.sestavljen x Vektor.prazen)


def glava : {A : Type} → {n : Nat} → Vektor A n → Option A :=
  fun vec => match vec with
  | Vektor.prazen => Option.none
  | Vektor.sestavljen x _ => Option.some x


def rep : {A : Type} → {n : Nat} → Vektor A n → Option A :=
  fun vec => glava (obrni vec)

/------------------------------------------------------------------------------
 ## Predikatni račun

 Dokažite spodnje tri trditve. Zadnja je _paradoks pivca_, ki pravi:
   "V vsaki neprazni gostilni obstaja gost, za katerega velja,
   da če pije on, pijejo vsi v gostilni."
 Za dokaz potrebujete klasično logiko, torej nekaj iz modula `Classical`.
------------------------------------------------------------------------------/

theorem forall_implies : {A : Type} → {P Q : A → Prop} →
  (∀ x, (P x → Q x)) → (∀ x, P x) → (∀ x, Q x) := by
  intro x P Q t1 t2 xt
  apply t1
  apply t2



theorem forall_implies' : {A : Type} → {P : Prop} → {Q : A → Prop} →
  (∀ x, (P → Q x)) ↔ (P → ∀ x, Q x) := by
  intro x P xP
  constructor

  intro t1 P_1 x_1
  apply t1
  assumption

  intro t2 P_2 x_2
  apply t2
  assumption


theorem paradoks_pivca :
  {G : Type} → {P : G → Prop} →
  (g : G) →  -- (g : G) pove, da je v gostilni vsaj en gost
  ∃ (p : G), (P p → ∀ (x : G), P x) := by
  intro Ljudje_v_gostilni    Ce_je_gost_v_gostilni    Gost_iz_gostilne
  constructor

  intro gost xl

  sorry

/------------------------------------------------------------------------------
 ## Dvojiška drevesa

 Podan naj bo tip dvojiških dreves skupaj s funkcijama za zrcaljenje in izračun
 višine ter dvema funkcijama, ki obe od leve proti desni naštejeta elemente
 drevesa. Pri tem prva deluje naivno in ima časovno zahtevnost O(n log n), druga
 pa je malo bolj zapletena in deluje v času O(n). Dokažite spodnje enakosti, pri
 čemer lahko do pomožne funkcije `aux` dostopate kot `elementi'.aux`
-------------------------------------------------------------------------------/

inductive Drevo : Type → Type where
  | prazno : {A : Type} → Drevo A
  | sestavljeno : {A : Type} → Drevo A → A → Drevo A → Drevo A

def zrcali : {A : Type} → Drevo A → Drevo A :=
  fun t => match t with
  | .prazno => .prazno
  | .sestavljeno l x d => .sestavljeno (zrcali d) x (zrcali l)

def visina : {A : Type} → Drevo A → Nat :=
  fun t => match t with
  | .prazno => 0
  | .sestavljeno l _ d => 1 + max (visina l) (visina d)

def elementi : {A : Type} → Drevo A → List A :=
  fun t => match t with
  | .prazno => []
  | .sestavljeno l x d => elementi l ++ x :: elementi d

def elementi' : {A : Type} → Drevo A → List A :=
  let rec aux : {A : Type} → Drevo A → List A → List A :=
    fun t acc => match t with
    | .prazno => acc
    | .sestavljeno l x d => aux l (x :: aux d acc)
  fun t => aux t []

theorem zrcali_zrcali :
  {A : Type} → (t : Drevo A) →
  zrcali (zrcali t) = t := by
  intro A t
  induction t with
  | prazno=>
    simp [zrcali]
  | sestavljeno h A1 A2 ih1 ih2 =>
    simp [zrcali]
    simp [ih1, ih2]


theorem visina_zrcali :
  {A : Type} → (t : Drevo A) →
  visina (zrcali t) = visina t := by
  intro A t
  induction t with
  | prazno=>
  simp [zrcali]
  | sestavljeno h A1 A2 ih1 ih2 =>
  simp [visina]
  simp [ih1, ih2]
  simp [Nat.max_comm]

theorem elementi'_aux_pomozno : {A : Type} → (h1 h2: Drevo A) →  (tp : A) →
  (elementi'.aux h1 [] ++ tp :: elementi'.aux h2 []) = elementi'.aux h1 (tp :: elementi'.aux h2 []) :=
    by
    intro A h1
    induction h1 with
    | prazno =>
    intro h2 tp
    simp [elementi'.aux]
    | sestavljeno l x d ih_l ih_d =>
    intro h2 tp
    simp [elementi'.aux]
    simp [← ih_d]
    rw[← elementi'.aux]
    rw [List.bind_assoc]


theorem elementi_elementi' :
  {A : Type} → (t : Drevo A) →
  elementi t = elementi' t := by
  intro A t
  induction t with
  | prazno=>
  simp [elementi,elementi', elementi'.aux]
  | sestavljeno h1 tp h2 ih1 ih2 =>
  simp [elementi, elementi']
  rw[ih1, ih2]
  simp [elementi'.aux, elementi']
  simp[elementi'_aux_pomozno]
