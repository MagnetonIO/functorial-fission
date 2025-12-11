# Functorial Fission: A Categorical Formulation of Nuclear Fission Energy

A rigorous mathematical and categorical treatment of nuclear fission energy, combining classical nuclear physics with category theory and typed functional programming.

## Overview

This repository contains an arXiv-style paper presenting a novel **functorial formulation** of nuclear fission physics. While the underlying physics (mass-energy equivalence, binding energy, chain reactions) is well-established, this work provides a structural reformulation using:

- **Category Theory**: Nuclei as objects in a monoidal category, fission as morphisms, binding energy as a functor
- **Type Theory**: Haskell implementations encoding physical invariants at the type level
- **Coalgebraic Semantics**: Neutron branching modeled as coalgebras capturing chain reaction dynamics

## Repository Structure

```
functorial-fission/
├── latex/                 # LaTeX source files
│   └── main.tex          # Main paper source
├── code/                  # Haskell implementation
│   └── src/
│       └── FunctorialFission.hs
├── pdf/                   # Compiled PDF output
├── docs/                  # GitHub Pages site
└── README.md
```

## Key Contributions

1. **Categorical Framework**: Nuclear states form a symmetric monoidal category with fission as structure-preserving morphisms
2. **Binding Energy Functor**: A functor B: Nucl → Energy mapping nuclear configurations to their binding energies
3. **Conservation as Natural Transformations**: Charge, baryon number, and energy conservation emerge as natural transformations
4. **Coalgebraic Chain Reactions**: Neutron multiplication modeled as coalgebras over the branching functor

## Building

### LaTeX Paper
```bash
cd latex
pdflatex main.tex
bibtex main
pdflatex main.tex
pdflatex main.tex
```

### Haskell Code
```bash
cd code
cabal build
```

## Citation

If you use this work, please cite:
```bibtex
@article{long2025functorial,
  title={A Functorial Formulation of Nuclear Fission: Mass-Energy Equivalence,
         Binding Energy, and Chain-Reaction Dynamics in a Categorical Framework},
  author={Long, Matthew},
  journal={arXiv preprint},
  year={2025}
}
```

## License

MIT License - See LICENSE file for details.

## Author

**Matthew Long**
Research in categorical physics, typed functional programming, and formal methods.
