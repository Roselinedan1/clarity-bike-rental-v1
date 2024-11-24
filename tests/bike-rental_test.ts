import {
  Clarinet,
  Tx,
  Chain,
  Account,
  types
} from 'https://deno.land/x/clarinet@v1.0.0/index.ts';
import { assertEquals } from 'https://deno.land/std@0.90.0/testing/asserts.ts';

Clarinet.test({
  name: "Ensure that bike registration works correctly",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get('deployer')!;
    const wallet1 = accounts.get('wallet_1')!;
    
    let block = chain.mineBlock([
      Tx.contractCall('bike-rental', 'register-bike', [], deployer.address),
      Tx.contractCall('bike-rental', 'register-bike', [], wallet1.address)
    ]);
    
    block.receipts[0].result.expectOk();
    block.receipts[1].result.expectErr(types.uint(100)); // err-owner-only
  },
});

Clarinet.test({
  name: "Test complete rental cycle",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get('deployer')!;
    const renter = accounts.get('wallet_1')!;
    
    let block = chain.mineBlock([
      // Register bike
      Tx.contractCall('bike-rental', 'register-bike', [], deployer.address),
      
      // Rent bike
      Tx.contractCall('bike-rental', 'rent-bike', [
        types.principal(deployer.address)
      ], renter.address),
      
      // Return bike
      Tx.contractCall('bike-rental', 'return-bike', [
        types.principal(deployer.address)
      ], renter.address)
    ]);
    
    block.receipts.forEach(receipt => {
      receipt.result.expectOk();
    });
  },
});

Clarinet.test({
  name: "Test unauthorized return fails",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get('deployer')!;
    const renter = accounts.get('wallet_1')!;
    const unauthorized = accounts.get('wallet_2')!;
    
    let block = chain.mineBlock([
      Tx.contractCall('bike-rental', 'register-bike', [], deployer.address),
      Tx.contractCall('bike-rental', 'rent-bike', [
        types.principal(deployer.address)
      ], renter.address),
      Tx.contractCall('bike-rental', 'return-bike', [
        types.principal(deployer.address)
      ], unauthorized.address)
    ]);
    
    block.receipts[2].result.expectErr(types.uint(104)); // err-unauthorized
  },
});
