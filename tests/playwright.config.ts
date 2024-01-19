import { defineConfig, devices } from '@playwright/test';

// https://playwright.dev/docs/test-configuration.
export default defineConfig({
	testDir: './specs',
	fullyParallel: true,
	forbidOnly: Boolean(process.env.CI),
	retries: process.env.CI ? 2 : 0,
	workers: process.env.CI ? 1 : undefined,
	// https://playwright.dev/docs/test-reporters
	reporter: process.env.CI ? 'list' : 'html',
	// https://playwright.dev/docs/api/class-testoptions.
	use: {
		trace: 'on-first-retry'
	},

	projects: [
		{
			name: 'chromium',
			use: { ...devices['Desktop Chrome'] }
		},

		{
			name: 'firefox',
			use: { ...devices['Desktop Firefox'] }
		},

		{
			name: 'webkit',
			use: { ...devices['Desktop Safari'] }
		}
	]
});
