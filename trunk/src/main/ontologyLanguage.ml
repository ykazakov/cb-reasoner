(* provides a list of ontology languages and a function to determine the   *)
(* language from ontology statistics                                       *)

open Owl2
module O = Ontology
module PC = Polarity.Counter

type t =
  | EL
  | ELI
  | ELH
  | ELIH
  | ELF
  | ELIF
  | ELHF
  | ELIHF
  | ELT
  | ELIT
  | ELHT
  | ELIHT
  | ELFT
  | ELIFT
  | ELHFT
  | ELIHFT
  | EL_bot
  | ELI_bot
  | ELH_bot
  | ELIH_bot
  | ELF_bot
  | ELIF_bot
  | ELHF_bot
  | ELIHF_bot
  | ELT_bot
  | ELIT_bot
  | ELHT_bot
  | ELIHT_bot
  | ELFT_bot
  | ELIFT_bot
  | ELHFT_bot
  | ELIHFT_bot
  | FL0
  | FL0I
  | FL0H
  | FL0IH
  | FL0F
  | FL0IF
  | FL0HF
  | FL0IHF
  | FL0T
  | FL0IT
  | FL0HT
  | FL0IHT
  | FL0FT
  | FL0IFT
  | FL0HFT
  | FL0IHFT
  | AL
  | ALI
  | ALH
  | ALIH
  | ALF
  | ALIF
  | ALHF
  | ALIHF
  | ALT
  | ALIT
  | ALHT
  | ALIHT
  | ALFT
  | ALIFT
  | ALHFT
  | ALIHFT
  | ALC
  | ALCI
  | ALCH
  | ALCIH
  | ALCF
  | ALCIF
  | ALCHF
  | ALCIHF
  | ALCT
  | ALCIT
  | ALCHT
  | ALCIHT
  | ALCFT
  | ALCIFT
  | ALCHFT
  | ALCIHFT

let str = function
  | EL -> "EL"
  | ELI -> "ELI"
  | ELH -> "ELH"
  | ELIH -> "ELIH"
  | ELF -> "ELF"
  | ELIF -> "ELIF"
  | ELHF -> "ELHF"
  | ELIHF -> "ELIHF"
  | ELT -> "ELT"
  | ELIT -> "ELIT"
  | ELHT -> "ELHT"
  | ELIHT -> "ELIHT"
  | ELFT -> "ELFT"
  | ELIFT -> "ELIFT"
  | ELHFT -> "ELHFT"
  | ELIHFT -> "ELIHFT"
  | EL_bot -> "EL_bot"
  | ELI_bot -> "ELI_bot"
  | ELH_bot -> "ELH_bot"
  | ELIH_bot -> "ELIH_bot"
  | ELF_bot -> "ELF_bot"
  | ELIF_bot -> "ELIF_bot"
  | ELHF_bot -> "ELHF_bot"
  | ELIHF_bot -> "ELIHF_bot"
  | ELT_bot -> "ELT_bot"
  | ELIT_bot -> "ELIT_bot"
  | ELHT_bot -> "ELHT_bot"
  | ELIHT_bot -> "ELIHT_bot"
  | ELFT_bot -> "ELFT_bot"
  | ELIFT_bot -> "ELIFT_bot"
  | ELHFT_bot -> "ELHFT_bot"
  | ELIHFT_bot -> "ELIHFT_bot"
  | FL0 -> "FL0"
  | FL0I -> "FL0I"
  | FL0H -> "FL0H"
  | FL0IH -> "FL0IH"
  | FL0F -> "FL0F"
  | FL0IF -> "FL0IF"
  | FL0HF -> "FL0HF"
  | FL0IHF -> "FL0IHF"
  | FL0T -> "FL0T"
  | FL0IT -> "FL0IT"
  | FL0HT -> "FL0HT"
  | FL0IHT -> "FL0IHT"
  | FL0FT -> "FL0FT"
  | FL0IFT -> "FL0IFT"
  | FL0HFT -> "FL0HFT"
  | FL0IHFT -> "FL0IHFT"
  | AL -> "AL"
  | ALI -> "ALI"
  | ALH -> "ALH"
  | ALIH -> "ALIH"
  | ALF -> "ALF"
  | ALIF -> "ALIF"
  | ALHF -> "ALHF"
  | ALIHF -> "ALIHF"
  | ALT -> "ALT"
  | ALIT -> "ALIT"
  | ALHT -> "ALHT"
  | ALIHT -> "ALIHT"
  | ALFT -> "ALFT"
  | ALIFT -> "ALIFT"
  | ALHFT -> "ALHFT"
  | ALIHFT -> "SHIF"
  | ALC -> "ALC"
  | ALCI -> "ALCI"
  | ALCH -> "ALCH"
  | ALCIH -> "ALCIH"
  | ALCF -> "ALCF"
  | ALCIF -> "ALCIF"
  | ALCHF -> "ALCHF"
  | ALCIHF -> "ALCIHF"
  | ALCT -> "ALCT"
  | ALCIT -> "ALCIT"
  | ALCHT -> "ALCHT"
  | ALCIHT -> "ALCIHT"
  | ALCFT -> "ALCFT"
  | ALCIFT -> "ALCIFT"
  | ALCHFT -> "ALCHFT"
  | ALCIHFT -> "SHIF"

let is_horn_lang = function
  | EL -> true
  | ELI -> true
  | ELH -> true
  | ELIH -> true
  | ELF -> true
  | ELIF -> true
  | ELHF -> true
  | ELIHF -> true
  | ELT -> true
  | ELIT -> true
  | ELHT -> true
  | ELIHT -> true
  | ELFT -> true
  | ELIFT -> true
  | ELHFT -> true
  | ELIHFT -> true
  | EL_bot -> true
  | ELI_bot -> true
  | ELH_bot -> true
  | ELIH_bot -> true
  | ELF_bot -> true
  | ELIF_bot -> true
  | ELHF_bot -> true
  | ELIHF_bot -> true
  | ELT_bot -> true
  | ELIT_bot -> true
  | ELHT_bot -> true
  | ELIHT_bot -> true
  | ELFT_bot -> true
  | ELIFT_bot -> true
  | ELHFT_bot -> true
  | ELIHFT_bot -> true
  | _ -> false
;;

type opt =
  | Y    (* yes *)
  | N    (* no *)
;;

let is_horn ont =
  (PC.get_neg (O.count_ObjectComplementOf ont)) +
  (PC.get_pos (O.count_ObjectUnionOf ont)) +
  (PC.get_neg (O.count_ObjectAllValuesFrom ont)) == 0
;;

let non_horn_expl ont =
  let symb = ref " " in
  "the ontology contains" ^
  (if PC.get_neg (O.count_ObjectComplementOf ont) > 0 then
      let s = !symb ^ "negative object complements" in symb:=", "; s else "") ^
  (if PC.get_pos (O.count_ObjectUnionOf ont) > 0 then
      let s = !symb ^ "positive disjunctions" in symb:=", "; s else "") ^
  (if PC.get_neg (O.count_ObjectAllValuesFrom ont) > 0 then
      let s = !symb ^ "negative object all values from" in symb:=", "; s else "") ^ "."
;;

let expressivity ont =
  let has_c_neg = if O.total_ObjectComplementOf ont > 0 then Y else N in
  let has_c_conj = if O.total_ObjectIntersectionOf ont > 0 then Y else N in
  let has_c_disj = if O.total_ObjectUnionOf ont > 0 then Y else N in
  let has_exist = if O.total_ObjectSomeValuesFrom ont > 0 then Y else N in
  let has_univ = if O.total_ObjectAllValuesFrom ont > 0 then Y else N in
  let has_inv = if O.total_InverseObjectProperty ont > 0 || O.total_InverseProperties ont > 0
    then Y else N in
  let has_hier = if O.total_SubPropertyOf ont > 0 then Y else N in
  let has_trans = if O.total_TransitiveProperty ont > 0 then Y else N in
  let has_funct = if O.total_FunctionalProperty ont > 0 then Y else N in
  let has_bottom = if O.has_positive_Nothing ont then Y else N in
  
  match has_c_neg,
  has_c_conj, has_c_disj,
  has_exist, has_univ,
  has_inv, has_hier,
  has_funct, has_trans,
  has_bottom with
  | N, _, N, _, N, N, N, N, N, N -> EL
  | N, _, N, _, N, _, N, N, N, N -> ELI
  | N, _, N, _, N, N, _, N, N, N -> ELH
  | N, _, N, _, N, _, _, N, N, N -> ELIH
  | N, _, N, _, N, N, N, _, N, N -> ELF
  | N, _, N, _, N, _, N, _, N, N -> ELIF
  | N, _, N, _, N, N, _, _, N, N -> ELHF
  | N, _, N, _, N, _, _, _, N, N -> ELIHF
  | N, _, N, _, N, N, N, N, _, N -> ELT
  | N, _, N, _, N, _, N, N, _, N -> ELIT
  | N, _, N, _, N, N, _, N, _, N -> ELHT
  | N, _, N, _, N, _, _, N, _, N -> ELIHT
  | N, _, N, _, N, N, N, _, _, N -> ELFT
  | N, _, N, _, N, _, N, _, _, N -> ELIFT
  | N, _, N, _, N, N, _, _, _, N -> ELHFT
  | N, _, N, _, N, _, _, _, _, N -> ELIHFT
  | N, _, N, _, N, N, N, N, N, _ -> EL_bot
  | N, _, N, _, N, _, N, N, N, _ -> ELI_bot
  | N, _, N, _, N, N, _, N, N, _ -> ELH_bot
  | N, _, N, _, N, _, _, N, N, _ -> ELIH_bot
  | N, _, N, _, N, N, N, _, N, _ -> ELF_bot
  | N, _, N, _, N, _, N, _, N, _ -> ELIF_bot
  | N, _, N, _, N, N, _, _, N, _ -> ELHF_bot
  | N, _, N, _, N, _, _, _, N, _ -> ELIHF_bot
  | N, _, N, _, N, N, N, N, _, _ -> ELT_bot
  | N, _, N, _, N, _, N, N, _, _ -> ELIT_bot
  | N, _, N, _, N, N, _, N, _, _ -> ELHT_bot
  | N, _, N, _, N, _, _, N, _, _ -> ELIHT_bot
  | N, _, N, _, N, N, N, _, _, _ -> ELFT_bot
  | N, _, N, _, N, _, N, _, _, _ -> ELIFT_bot
  | N, _, N, _, N, N, _, _, _, _ -> ELHFT_bot
  | N, _, N, _, N, _, _, _, _, _ -> ELIHFT_bot
  | N, _, N, N, _, N, N, N, N, _ -> FL0
  | N, _, N, N, _, _, N, N, N, _ -> FL0I
  | N, _, N, N, _, N, _, N, N, _ -> FL0H
  | N, _, N, N, _, _, _, N, N, _ -> FL0IH
  | N, _, N, N, _, N, N, _, N, _ -> FL0F
  | N, _, N, N, _, _, N, _, N, _ -> FL0IF
  | N, _, N, N, _, N, _, _, N, _ -> FL0HF
  | N, _, N, N, _, _, _, _, N, _ -> FL0IHF
  | N, _, N, N, _, N, N, N, _, _ -> FL0T
  | N, _, N, N, _, _, N, N, _, _ -> FL0IT
  | N, _, N, N, _, N, _, N, _, _ -> FL0HT
  | N, _, N, N, _, _, _, N, _, _ -> FL0IHT
  | N, _, N, N, _, N, N, _, _, _ -> FL0FT
  | N, _, N, N, _, _, N, _, _, _ -> FL0IFT
  | N, _, N, N, _, N, _, _, _, _ -> FL0HFT
  | N, _, N, N, _, _, _, _, _, _ -> FL0IHFT
  | N, _, N, _, _, N, N, N, N, _ -> AL
  | N, _, N, _, _, _, N, N, N, _ -> ALI
  | N, _, N, _, _, N, _, N, N, _ -> ALH
  | N, _, N, _, _, _, _, N, N, _ -> ALIH
  | N, _, N, _, _, N, N, _, N, _ -> ALF
  | N, _, N, _, _, _, N, _, N, _ -> ALIF
  | N, _, N, _, _, N, _, _, N, _ -> ALHF
  | N, _, N, _, _, _, _, _, N, _ -> ALIHF
  | N, _, N, _, _, N, N, N, _, _ -> ALT
  | N, _, N, _, _, _, N, N, _, _ -> ALIT
  | N, _, N, _, _, N, _, N, _, _ -> ALHT
  | N, _, N, _, _, _, _, N, _, _ -> ALIHT
  | N, _, N, _, _, N, N, _, _, _ -> ALFT
  | N, _, N, _, _, _, N, _, _, _ -> ALIFT
  | N, _, N, _, _, N, _, _, _, _ -> ALHFT
  | N, _, N, _, _, _, _, _, _, _ -> ALIHFT
  | _, _, _, _, _, N, N, N, N, _ -> ALC
  | _, _, _, _, _, _, N, N, N, _ -> ALCI
  | _, _, _, _, _, N, _, N, N, _ -> ALCH
  | _, _, _, _, _, _, _, N, N, _ -> ALCIH
  | _, _, _, _, _, N, N, _, N, _ -> ALCF
  | _, _, _, _, _, _, N, _, N, _ -> ALCIF
  | _, _, _, _, _, N, _, _, N, _ -> ALCHF
  | _, _, _, _, _, _, _, _, N, _ -> ALCIHF
  | _, _, _, _, _, N, N, N, _, _ -> ALCT
  | _, _, _, _, _, _, N, N, _, _ -> ALCIT
  | _, _, _, _, _, N, _, N, _, _ -> ALCHT
  | _, _, _, _, _, _, _, N, _, _ -> ALCIHT
  | _, _, _, _, _, N, N, _, _, _ -> ALCFT
  | _, _, _, _, _, _, N, _, _, _ -> ALCIFT
  | _, _, _, _, _, N, _, _, _, _ -> ALCHFT
  | _, _, _, _, _, _, _, _, _, _ -> ALCIHFT
;;

let str_expressivity ont =
  let lang = expressivity ont in
  if is_horn_lang lang then str lang else
  if is_horn ont then "Horn-" ^ (str lang)
  else
    (str lang) ^ ", non-Horn: " ^
    (non_horn_expl ont)
;;