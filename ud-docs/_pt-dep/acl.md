---
layout: relation
title: 'acl'
shortdef: 'clausal modifier of noun (adjectival clause)'
---

`acl` stands for finite and non-finite clauses that modify a nominal.  The `acl` relation 
contrasts with the [advcl]() relation, which is used for adverbial clauses
that modify a predicate. The head of the `acl` relation is the noun
that is modified, and the dependent is the head of the clause that
modifies the noun.

In Portuguese, there are also 2 other language-specific subtypes of `acl`: `acl:part`, `acl:relcl`.

Examples:

"Uma defesa bem parecida com aquela de Lula, no começo das denúncias contra José Paulo Bisol."

acl(defesa, parecida)

"O caso nasceu de uma «vendetta», obra e desgraça de um certo Francesco Farina, dono do Modena, um time rebaixado à 3ª divisão."

acl(time, rebaixado)

"A Liga de Assistência e Recuperação, órgão ligado à Prefeitura de Salvador, está desenvolvendo um projeto para a confecção de brinquedos a partir de sucatas."

acl(orgão, ligado)
