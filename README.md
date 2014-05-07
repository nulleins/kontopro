kontopro
========

Sample Account domain object demonstrating immutability, event-sourcing and projection

Explanation of this experiment: the domain concept modelled here, Account (actually, a Stored-Value Account), is an
Aggregate Root \[AR\] in the system: that is, it is the focus of specific business-level transactions that effect its
state.

As we are trying to achieve low-latency, high-throughput and bug-free processing, we should try and keep this entity
immutable, much like a value object, but that prevents doing any useful work with it; the solution is to keep
the entity immutable but on method application, return a new instance with the data changed, much like BigDecimal.add()
for example.

This approach throws up one problem: an entity does not necessarily have all its data set on creation, so this would
leave it in a possibly invalid state initially (e.g., before an account number has been assigned); to avoid this, and
at the same time model the fact that an account evolves through different (summary) states (it's life-cycle), the
following code uses sub-classes of account to represent the states (e.g., OpenAccount, ClosedAccount) and only these
sub-classes have the new state fields required by their summary state.  In addition, if we have the business rule that
only an open account is available for deposits and withdrawals, this can be enforced in code by only providing these
methods on the OpenAccount sub-class (a "correctness by construction" policy)

The other interesting aspect to this experiment is that the business methods on the entities in fact do nothing more
than generate a sequence of events: the client (normally, an application service), to execute business logic on an entity
should first retrieve the entity from its repository (by Id), perform its own validity checks (that is, things that the
entity itself cannot be expected to check) and then call the method(s) on the entity; this will return a sequence of
DomainEvent and/or Error Events, which the application service will save to the repository (so the next time the entity
is retrieved, it will be reconstructed with the state changes specified by the event(s)), and publish to the internal
event bus, allowing interested sub-components of the system to react to the change, if required: this would also include
operational notification of error events, and construction of the read-model for the entity, given a CQRS architecture.

The Account entity has a companion object that acts as a factory for Accounts: given an event stream detailing the
historical events that have happened to an Account instance, the factory will reconstruct the entity to its current state
(projection); this is accomplished via a left-fold operation on the sequence, successively applying each event to the
previously-constructed Account entity (`applyEvent ( account: Account, event: AccountEvent)`)

In the Account class, public methods designed to be called by an application or domain service of of the type
`f: Seq[DomainEvent]`, and internal state-effecting methods, used by the event applying factory, are of visibility
`protected[model]` and return a new instance of the entity: these internal methods should protect the entity's
invariant via assertions (require clauses), whereas the public methods should return an error event specifying the
invalid application

\[AR\] (Evans): An AGGREGATE is a cluster of associated objects that we treat as a unit for the
purpose of data changes. Each AGGREGATE has a root and a boundary. The boundary defines what
is inside the AGGREGATE. The root is a single, specific ENTITY contained in the AGGREGATE.
The root is the only member of the AGGREGATE that outside objects are allowed to hold references to
