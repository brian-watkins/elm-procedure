const { test, expect } = require('@playwright/test');

test("runs a procedure with an open channel upon init", async ({ page }) => {
  await page.goto("http://localhost:9732");

  await page.keyboard.type("bbb")

  await expect(page.locator("[data-on-type]")).toContainText("You have not yet pressed X, Y, or Z")

  await page.keyboard.type("Z")

  await expect(page.locator("[data-on-type]")).toContainText("You pressed Z!!!")

  await page.keyboard.type("X")

  await expect(page.locator("[data-on-type]")).toContainText("You pressed X!!!")
})

test("runs a procedure that makes a request to a port and processes the response asynchronously", async ({ page }) => {
  await page.goto("http://localhost:9732")

  await page.type("[data-port-input]", "Hello async!")

  await page.click("[data-port-async-submit]")

  await expect(page.locator("[data-port-message]")).toContainText("Thanks for the message: Hello async!")
})

test("runs a procedure that makes a request to a port and processes the response synchronously", async ({ page }) => {
  await page.goto("http://localhost:9732")

  await page.type("[data-port-input]", "Hello synchronous!")

  await page.click("[data-port-sync-submit]")

  await expect(page.locator("[data-port-message]")).toContainText("Thanks for the message: Hello synchronous!")
})

test("runs multiple procedures that use the same ports in different ways", async ({ page }) => {
  await page.goto("http://localhost:9732")

  await page.locator("[data-port-input]").fill("Hello")

  await page.click("[data-word-save]")

  await page.locator("[data-port-input]").fill("27")

  await page.click("[data-number-save]")

  await expect(page.locator("[data-save-messages]")).toContainText("You saved a word: Hello")
  await expect(page.locator("[data-save-messages]")).toContainText("You saved a number: 27")
})