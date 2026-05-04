# Manage Categories

## Goal
As a user, I want a stable category tree with built-in and personal categories, so I can classify transactions using durable category ids.

## Behavior And Business Rules
- The backend must expose one effective category tree per authenticated user.
- The effective tree includes backend-owned built-in categories and user-owned categories.
- Each category may have at most one parent in v1.
- Built-in categories are read-only.
- User-owned categories can be created, updated, and deleted.
- No automatic or default category assignment exists in v1.
- Top-level built-in categories are not selectable.
- Any non-top-level category is selectable, even when it has children.
- User-owned categories may be created under built-in or user-owned parents.
- Deleting a user-owned category is allowed only when it has no child categories and is not referenced by current transaction categorization or active split state.

## Data And Contracts
- Defines `GET /api/v1/finance/categories`, `POST /api/v1/finance/categories`, `POST /api/v1/finance/categories/{id}`, and `DELETE /api/v1/finance/categories/{id}`.
- Category rows expose `id`, `name`, `parentId`, `owner`, and `selectable`.
- `POST /api/v1/finance/categories` and `POST /api/v1/finance/categories/{id}` accept `name` and optional `parentId`.
- Built-in categories remain read-only through public write endpoints and reject update or delete with `409`.
- Invalid parent ids, cross-user parent references, and attempted parent cycles return `400`.
- Deleting a referenced user-owned category returns `409`.
- Deleting a user-owned category that still has child categories returns `409`.
- The canonical v1 built-in category tree is:

```text
Income
  Salary
  Freelance
  Reimbursement
  Gift
  Interest

Housing
  Rent / Mortgage
  Utilities
  Internet
  Home Insurance
  Maintenance
  Furniture

Food
  Groceries
  Restaurants
  Coffee / Snacks
  Delivery

Transport
  Public Transport
  Fuel
  Parking
  Taxi / Ride Share
  Vehicle Maintenance
  Vehicle Insurance

Health
  Doctor
  Pharmacy
  Health Insurance
  Therapy

Personal
  Clothing
  Education
  Books
  Subscriptions
  Digital Services

Household
  Cleaning Supplies
  Appliances
  Tools
  Home Goods

Pets
  Food
  Veterinary
  Toys
  Training

Leisure
  Entertainment
  Leisure Travel
  Hobbies
  Sports
  Games
  Garden

Family & Social
  Gifts
  Donations
  Childcare
  Events

Financial
  Bank Fees
  Taxes

Professional
  Equipment
  Software
  Professional Travel
  Professional Meals
  Training
  Services

Adjustments
  Refund

Uncategorized
  Uncategorized Expense
  Uncategorized Income
```

## Technical Details
- Category ids must remain stable so transaction classification history remains durable.
- The effective category tree should combine built-in and user-owned categories in one authenticated read model.
- Built-in category ids should be seeded deterministically as public slug ids as part of the backend contract.
- Selection validation must reject only top-level built-in categories, not all non-leaf categories.
- Reference checks for delete must cover active whole-transaction categorization and active split state.
- No built-in transfer category exists in v1 because internal movement is modeled through explicit transfer links.
- No built-in `Reversal`, `Savings`, or `Investments` category exists in v1.

## Testing
- Reading categories returns the effective tree for the authenticated user.
- Top-level built-in categories are returned as non-selectable.
- Built-in child categories are returned as selectable.
- Creating a user-owned category succeeds with no parent, a built-in parent, or a user-owned parent.
- Updating a user-owned category succeeds when the new parent is valid.
- Updating or deleting a built-in category through public write endpoints returns `409`.
- Creating or updating a category with an invalid parent, cross-user parent, or parent cycle returns `400`.
- Deleting a referenced user-owned category returns `409`.
- Deleting a user-owned category with child categories returns `409`.
- Deleting an unreferenced user-owned category succeeds.

## Rollout And Compatibility
- Backward-compatible because this is a new API surface.
- Any built-in category seed or schema introduction should be additive.
