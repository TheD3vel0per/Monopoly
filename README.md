# Monopoly

**Game State**:
- Players
	- Which "items" they are
	- Turns of 
	- Jail & remaining time
- Properties
	- Houses
	- Owner

**Actions**:
- Roll Dice
- Get Salary?
    - If we pass go, get $200
- Land On Tile
	- If not owned
		- Buy
		- Auction
		- Pass
	- Otherwise 
		- Pay Rent
	- If its not a property then, pickup card?
		- Pay Something (Based on House / Hotels / Properties)
		- Get Money from some Players (all or some)
	- If you land on go to jail, go to jail
- Anytime
	- Trade Propose
		- Accept
		- Reject
		- Counter
	- Mortage your properties
- If you cant pay for something at any point then declare bankruptcy
- If you're in jail skip here,
	- Roll doubles and try to get out
	- Post bail
	- Stay 3 rounds
- End Turn

## MVP
@kaylaox this is what we're working on right now
Only roll the dice and land on properties, nothing more than that. When you land on properties you can only buy, pass or pay rent. Still collect $200 when going around.