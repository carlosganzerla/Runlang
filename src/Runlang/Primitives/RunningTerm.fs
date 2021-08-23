module RunningTerm

type RunningTerm =
    | CL
    | CA
    | CV
    | TR
    | LVS
    | LE
    | MO
    | FO
    | FTS
    | MAX

module RunningTerm =
    let toString (term: RunningTerm) = sprintf "%A" term
