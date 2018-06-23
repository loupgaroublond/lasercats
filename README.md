# LaserCats

PEW PEW PEW PEW PEW!!!

LaserCats is a prototype for a CQRS based configuration and infrastructure
management system. It is based on a simple concept, all infrastructure is a form
of state. State can be stored on disk, serialized as a log of commands and
events, queried, modified and stored on disk again. The state is a document that
describes the infrastructure and said document can be used to recreate the
infrastructure. CQRS makes that possible.

# Status

Lasercats is in the very early stages. I am currently prototyping the ability to
issue high level commands, interpret them into instructions, and then rewrite
history to derive an equivalent log of commands. Along the way, I will be
developing some rudimentary state management and storage, a rudimentary query
engine and a demonstration of multiple interpreters.

Most development will be focused on getting the types right and proceed slowly.

# Manifesto

## Commands and direct control

Infrastructure operators prefer having direct commands to operate their
infrastructure. Direct control is crucial to enable operators to change the
parameters of the systems they run quickly in response to changing conditions,
especially where automation has failed. Likewise, direct feedback is neccessary
for both skilled operators to continue maintaining control and operators
learning how to operate the system. Direct commands in CQRS facilitate this.

## Automation

Automation is one step removed from control but also crucial to manage large
scale systems and infrastructure. It enables operators to achieve more with less
work by leveraging what computers can do. The current trend is to describe
infrastructure as declarative code and data structures. Folding commands, events
and declarative documents enables operators to combine the flexibility of
individual commands with the need for broad automation.

## Interpretation

Systems and machines normally process and interpret commands through the lens of
their machinery. Allowing alternative interpretation of commands and events
enables operators to simulate changes to infrastructure, apply changes to test
environments first and even troubleshoot problems. A command and event log
enables operators to develop and use simpler interpreters.

## State

All infrastructure is a form of state, whether ingrained in software, such as
the state of EC2 instances, switch port configuration, or files on disk, or at a
hardware level, such as the configuration of a 48U rack. Commands change the
state, which creates events describing the change in state. This state can be
regenerated from events, with limitations, or be used to generate instructions
to recreate infrastructure.

## Querying

Commands are rarely issued blindly. Queries close the loop between commands and
feedback, enabling operators to issue commands dependent on the current state of
the system. Declarative infrastructure can be built as a Query-Command pair.

# License
   
Copyright 2018 Yaakov M Nemoy

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
