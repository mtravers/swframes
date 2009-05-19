;;; ordinary
(DO-SPARQL *bioblog-store*
  '(:SELECT
    (?TRIAL ?TITLE ?INTTYPE ?INTNAME)
    (:OFFSET 0 :LIMIT 20)
    (?TRIAL #$linkedct:condition ?CONDITION)
    (?CONDITION #$linkedct:condition_name "Leprosy")
    (?TRIAL #$linkedct:brief_title ?TITLE)
    (?TRIAL #$linkedct:intervention ?INTERVENTION)
    (?INTERVENTION #$linkedct:intervention_type ?INTTYPE)
    (?INTERVENTION #$linkedct:intervention_name ?INTNAME)))


;;; case-insensitive -- times out, it's not very smart apparently
(DO-SPARQL *bioblog-store*
  '(:SELECT
    (?TRIAL ?TITLE ?INTTYPE ?INTNAME)
    (:OFFSET 0 :LIMIT 20)
    (?TRIAL #$linkedct:condition ?CONDITION)
    (?CONDITION #$linkedct:condition_name ?cname)
    (?TRIAL #$linkedct:brief_title ?TITLE)
    (?TRIAL #$linkedct:intervention ?INTERVENTION)
    (?INTERVENTION #$linkedct:intervention_type ?INTTYPE)
    (?INTERVENTION #$linkedct:intervention_name ?INTNAME)
    (:filter (:regex ?cname "leprosy" "i"))))

;;; this works OK
(DO-SPARQL *bioblog-store*
  '(:SELECT
    (?TRIAL ?TITLE ?INTTYPE ?INTNAME)
    (:OFFSET 0 :LIMIT 20)
    (?TRIAL #$linkedct:condition ?CONDITION)
    (?CONDITION #$linkedct:condition_name ?cname)
    (?TRIAL #$linkedct:brief_title ?TITLE)
    (?TRIAL #$linkedct:intervention ?INTERVENTION)
    (?INTERVENTION #$linkedct:intervention_type ?INTTYPE)
    (?INTERVENTION #$linkedct:intervention_name ?INTNAME)
    (:filter (:regex ?cname "^leprosy$" "i"))))



(DO-SPARQL *bioblog-store*
  (case-insensitize
  '(:SELECT
    (?TRIAL ?TITLE ?INTTYPE ?INTNAME)
    (:OFFSET 0 :LIMIT 20)
    (?TRIAL #$linkedct:condition ?CONDITION)
    (?CONDITION #$linkedct:condition_name "Leprosy")
    (?TRIAL #$linkedct:brief_title ?TITLE)
    (?TRIAL #$linkedct:intervention ?INTERVENTION)
    (?INTERVENTION #$linkedct:intervention_type ?INTTYPE)
    (?INTERVENTION #$linkedct:intervention_name ?INTNAME))))
