# sat-backend-dpll

[sat-protocol](https://github.com/egao1980/sat-protocol) backend: DPLL with unit propagation, pure-literal elimination and decision heuristics (pluggable decision order). A CDCL backend comes later.

**Status:** planned (slice L2, Track L, after sat-protocol 0.1.0). Brief: [cl-stack/docs/capabilities/logic.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/logic.md).

Loading `sat-backend-dpll` makes a `dpll-backend` the default `sat-protocol:*sat-backend*` when none is set. `(make-instance 'dpll-backend :heuristic :first :pure-literals nil)` selects the plain first-variable search; `dpll-backend-statistics` reports decisions, propagations, pure literals and conflicts of the last call. Tests: sat-protocol's conformance suite plus DPLL-specific tests.

## License

MIT, see [LICENSE](LICENSE).
